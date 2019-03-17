;;; org-tomato.el --- Org-Mode Pomodoro Implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Nathan Dwarshuis

;; Author: Nathan Dwarshuis <natedwarshuis@gmail.com>
;; Keywords: org-mode, data
;; Homepage: https://github.com/ndwarshuis/org-tomato
;; Package-Requires: ((emacs "25") (dash "2.15"))
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; complexifier is a real word

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dash)
(require 'org)
(require 'org-clock)

;;; custom variables
(defvar org-tomato-log-file "~/Org/pomodoro.org_archive"
  "The file in which org pomodoros are stored.")

(defvar org-tomato-pomodoro-length 1500
  "Time of a pomodoro in seconds.")

(defvar org-tomato-break-length 300
  "Time of a short break in seconds.")

(defvar org-tomato-long-break-length 1200
  "Time of a long break in seconds.")

(defvar org-tomato-cycles 4)

;; TODO, this is not a good way to list files...
(defvar org-tomato-files-ignore
  `(,(expand-file-name org-tomato-log-file)))

;;; internal variables
(defvar org-tomato--timer nil)

;;; macros
(defmacro org-tomato--with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body)) (indent 1))
  `(progn
     ,@(--map (cons #'advice-add it) adlist)
     (unwind-protect (progn ,@body)
       ,@(--map `(advice-remove ,(car it) ,(nth 2 it)) adlist))))

(defmacro org-tomato--goto (point &rest body)
  "If POINT is non-null, go to POINT and execute BODY.
Otherwise do nothing."
  `(when ,point (save-excursion (goto-char point) ,@body)))

(defmacro org-tomato--with-log-file (&rest body)
  "Open log file and execute BODY and return last result from BODY."
  `(let ((buffer (find-file-noselect org-tomato-log-file)))
     (with-current-buffer buffer ,@body)))

(defmacro org-tomato--with-log-file-save (&rest body)
  "Open log file and execute BODY.
Make log file if it does not exist and save when BODY has finished
execution."
  `(org-tomato--with-log-file
    (unless (file-exists-p org-tomato-log-file)
      (insert
       (string-join
        (list "#    -*- mode: org -*-\n"
              "\n"
              "# do not modify unless you know what you are doing\n"
              "\n"))))
    ,@body
    (let ((inhibit-message t)) (save-buffer))))

;;; advising hacks

(defun org-tomato--resolve-clocks-ad (orig-fn &optional only-dangling-p
                                             prompt-fn last-valid)
  "Add advice to ignore a list of files when resolving clocks."
  (org-tomato--with-advice
      ((#'org-files-list
        :around (lambda (f) (-difference (funcall f) org-tomato-files-ignore))))
    (unwind-protect (funcall orig-fn only-dangling-p prompt-fn last-valid))))

(advice-add #'org-resolve-clocks :around #'org-tomato--resolve-clocks-ad)

;;; state machine
;; Can't be fooled,
;; Won't be ruled,
;; By the tomato state machine

(cl-defstruct (org-tomato--state
               (:constructor org-tomato--state-create)
               (:copier nil))
  "An interval state for tomato mode."
  name timestamp)

(cl-defstruct (org-tomato--transition
               (:constructor org-tomato--transition-create)
               (:copier nil))
  "A transition for tomato mode."
  old-state new-state signal hooks)

(defun org-tomato--create-transition (old new sig &rest subs)
  "Create a state machine transition.
OLD and NEW are the names of the old and new state. SIG is the signal
to trigger the transition. SUBS are subroutines to be run with no
arguments when the transition is triggered."
  (org-tomato--transition-create
   :old-state old
   :new-state new
   :signal sig
   :hooks `(,@subs)))

(defvar org-tomato--transition-table nil
  "Transition table.")

(setq org-tomato--transition-table
      (--map
       (apply #'org-tomato--create-transition it)
       `((inactive
          active
          :clock-in
          org-tomato--clock-in
          ,(-partial #'org-tomato--message "active"))
         (break
          active
          :clock-in
          org-tomato--clock-in
          ,(-partial #'org-tomato--message "active"))
         (active
          break
          :clock-out
          org-tomato--clock-out
          ,(-partial #'org-tomato--message "break"))
         (active
          inactive
          :kill
          org-tomato--kill
          ,(-partial #'org-tomato--message "inactive"))
         (break
          inactive
          :kill
          org-tomato--kill
          ,(-partial #'org-tomato--message "inactive")))))

(defun org-tomato--next-state (sig state)
   "Run the tomato state machine with SIG and current STATE."
   (let ((trans
          (--find
           (and
            (eq (org-tomato--state-name state)
                (org-tomato--transition-old-state it))
            (eq sig
                (org-tomato--transition-signal it)))
           org-tomato--transition-table)))
     (if (not trans) state
       (let* ((new-state-name (org-tomato--transition-new-state trans))
              (hooks (org-tomato--transition-hooks trans))
              (new-state
               (org-tomato--state-create
                :name new-state-name
                :timestamp (float-time))))
         ;; perform side effects before returning new state
         ;; this guarentees that everything is ok before finalizing
         ;; the new state
         (--each hooks (funcall it))
         new-state))))

(defvar org-tomato--state nil
  "The tomato-mode transition table.")

(setq org-tomato--state (org-tomato--state-create :name 'inactive))

;;; logging hooks

(defun org-tomato--element-parse-headline (&optional subtree)
  "Parse one headline using `org-element-parse-buffer'.
Assumes that point is currently on the starting line of the headline
in question. If SUBTREE is t, return all the subheadings under this
heading."
   ;; (line-beginning-position)
  (let ((start (point))
        (end (if subtree
                 (save-excursion (org-end-of-subtree))
               (save-excursion (outline-next-heading) (point)))))
    (-> (org-element--parse-elements
         start end 'first-section nil nil nil nil)
        car)))

(defun org-tomato--message (state)
  "Send a message for new STATE."
  (message "New Org-Tomato state: %s" state))

(defun org-tomato--start-pomodoro-timer (&optional diff)
  "Start a pomodoro timer.
DIFF is amount of time to subtract from `org-tomato-pomodoro-length'."
  (let ((length (- org-tomato-pomodoro-length (or diff 0))))
    (org-tomato--set-timer length "Pomodoro Ended")))

(defun org-tomato--start-rest-timer (&optional long)
  "Start a break timer.
Use `org-tomato-long-break-length' when LONG, otherwise use
`org-tomato-break-length'."
  (if (not long)
      (org-tomato--set-timer org-tomato-break-length "Break Ended")
    (org-tomato--set-timer org-tomato-long-break-length "Long Break Ended")))

(defun org-tomato--cancel-timer ()
  "Cancel the currently running timer."
  (when (timerp org-tomato--timer)
    (cancel-timer org-tomato--timer))
  (setq org-tomato--timer nil))

(defun org-tomato--set-timer (future message)
  "Set a new timer FUTURE seconds into the future with MESSAGE."
  (org-tomato--cancel-timer)
  (setq org-tomato--timer
        (run-at-time future nil #'org-tomato--notify message)))

(defun org-tomato--notify (message)
  "Notify the user with MESSAGE."
  (org-notify message))

(defun org-tomato--read-past-seconds (past-clock)
  "Prompt user for elapsed minutes to complete open clocks.
Prompt defaults to 'now' - PAST-CLOCK and accepts any values less
than this. Returns integer of elapsed seconds (converted from
minutes."
  (let* ((elapsed (- (/ (round (float-time)) 60) (/ past-clock 60)))
         (keep))
    (if (= 0 elapsed) 0
      (while
          (progn
            (setq keep (read-number "Open clock found. Minutes to keep?" elapsed))
            (unless (<= keep elapsed)
              (message "Must be less than %s minutes." elapsed)
              (sit-for 1)
              t)))
      (* 60 keep))))

(defun org-tomato--set-closed-p (point)
  "Test if the set under POINT is closed."
  (org-tomato--goto
   point
   (let ((not-top (org-up-heading-safe)))
     (while not-top (setq not-top (org-up-heading-safe))))
   (equal "DONE" (org-get-todo-state))))

(defun org-tomato--sum-pomodoro-clock (point)
  "Return the sum of all clocks under headline at POINT.
Value is in integer in seconds or nil if no clocks are found."
  (org-tomato--goto
   point
   (let ((time-convert
          (lambda (hhmm)
            (--> (split-string hhmm ":")
                 (mapcar #'string-to-number it)
                 (-> (car it) (* 60) (+ (cadr it)) (* 60))))))
     (-->
      (org-tomato--element-parse-headline)
      (org-element-map it 'clock (lambda (c) (org-element-property :duration c)))
      (--map (funcall time-convert it) it)
      (cl-reduce '+ it)))))

(defun org-tomato--get-pomodoro-cycle (point)
  "Return the cycle number of the pomodoro headline at POINT.
If none found, return nil."
  (org-tomato--goto
   point
   (beginning-of-line)
   (save-match-data
     (when (looking-at "\\*\\* pomodoro \\([[:digit:]]\\)")
       (string-to-number (match-string 1))))))

(defun org-tomato--get-open-clock (point)
  "Return value of open clock under POINT in unix time.
Return nil if no open clock."
  (org-tomato--goto
   point
   (save-match-data
     ;; TODO need to restrict this to current headline
     (when (search-forward-regexp (concat "[ \t]*" org-keyword-time-regexp) nil t)
       (-some-->
        (org-element-context)
        (org-element-property :value it)
        (and (eq 'inactive (org-element-property :type it)) it)
        (org-element-property :raw-value it)
        (org-2ft it)
        (round it))))))

(defun org-tomato--start-clock (point)
  "Start the clock at the headline under POINT."
  (org-tomato--goto
   point
   (org-clock-find-position nil)
   (insert-before-markers "\n")
   (backward-char 1)
   (org-indent-line)
   (->> (float-time)
        (format-time-string (org-time-stamp-format t t))
        (concat org-clock-string " ")
        (insert))))

(defun org-tomato--finish-clock (point &optional elapsed)
  "Complete the open clock for the headline under POINT.
ELAPSED is the length of the clock in minutes; if given use this to
set the final clock time instead of the current time."
  (org-tomato--goto
   point
   (search-forward-regexp (concat "[ \t]*" org-keyword-time-regexp) nil t)
   (delete-region (point) (point-at-eol))
   (let* ((old-time (->> (org-element-context)
                         (org-element-property :value)
                         (org-element-property :raw-value)
                         (org-2ft)))
          (diff (or elapsed (->> old-time (- (float-time)) round)))
          (h (/ diff 3600))
          (m (/ (% diff 3600) 60)))
     (--> old-time
          (+ it diff)
          (format-time-string (org-time-stamp-format t t) it)
          (format "--%s => %2d:%02d" it h m)
          (insert it)))))

(defun org-tomato--find-last-pomodoro (point)
  "Return point of latest pomodoro in the set under POINT."
  (org-tomato--goto
   point
   (org-end-of-subtree)
   (search-backward-regexp "\\*\\* pomodoro [[:digit:]]" point t)))

(defun org-tomato--find-last-set ()
  "Return the point to the last set in the log file or nil if none."
  (save-excursion
    (goto-char (point-max))
    (search-backward-regexp "\\* \\(TODO\\|DONE\\) set" nil t)))

(defun org-tomato--insert-pomodoro (point n)
  "Insert a new pomodoro headline at POINT in `org-tomato-log-file'.
N is the current pomodoro cycle."
  (save-excursion
    (goto-char point)
    (insert (format "** pomodoro %s\n" n))))

(defun org-tomato--insert-set (point)
  "Insert a new set headline at POINT in `org-tomato-log-file'."
  (save-excursion (goto-char point) (insert "* TODO set\n")))

(defun org-tomato--close-set (point)
  "Close the headline under POINT."
  (org-tomato--goto
   point
   (let ((org-todo-log-states nil))
     (org-todo 'done))))

(defun org-tomato--clock-in ()
  "Clock into a pomodoro given old and new states O and N."
  (org-tomato--cancel-timer)
  (org-tomato--with-log-file-save
   (let ((set-point (org-tomato--find-last-set)))
     ;; insert new set if there are no sets or no open sets
     (when (or (not set-point) (org-tomato--set-closed-p set-point))
       (org-tomato--insert-set (point-max))
       (setq set-point (org-tomato--find-last-set)))

     ;; insert pomodoro at cycle 1 if there are no pomodoros in set
     (let ((pom-point (org-tomato--find-last-pomodoro set-point)))
       (if (not pom-point)
           (org-tomato--insert-pomodoro (point-max) 1)

         ;; else insert pomodoros/sets based on what is in current set
         (let ((cycle (org-tomato--get-pomodoro-cycle pom-point))
               (open-clock (org-tomato--get-open-clock pom-point)))

           ;; if open clock, prompt user for how much of the elapsed
           ;; time to keep
           (-some->> open-clock
                     (org-tomato--read-past-seconds)
                     (org-tomato--finish-clock pom-point))

           ;; determine if we need to insert new sets or pomodoros
           (cond
            ;; insert next pomodoro if no open clocks and set not full
            ((and (not open-clock) (< cycle org-tomato-cycles))
             (org-tomato--insert-pomodoro (point-max) (+ 1 cycle)))

            ;; possibly insert next pomodoro if clock is open, set
            ;; is not full, and we want to use the old set. Do nothing
            ;; if we want to use the last pomodoro
            ((and open-clock
                  (< cycle org-tomato-cycles)
                  (y-or-n-p "Open set detected. Use old set? "))
             (unless (y-or-n-p "Continue with last pomodoro? ")
               (org-tomato--insert-pomodoro (point-max) (+ 1 cycle))))

            ;; all other cases, close last set and insert new set
            (t
             (org-tomato--close-set set-point)
             (org-tomato--insert-set (point-max))
             (org-tomato--insert-pomodoro (point-max) 1))))))

     ;; after placing all sets/pomodoros, start clock and timer
     (org-tomato--start-clock (point-max))
     (-> (org-tomato--find-last-set)
         org-tomato--find-last-pomodoro
         org-tomato--sum-pomodoro-clock
         org-tomato--start-pomodoro-timer))))

(defun org-tomato--clock-out ()
  "Clock out a pomodoro and close the set if complete."
  (org-tomato--cancel-timer)
  (org-tomato--with-log-file-save
   (let ((set-point (org-tomato--find-last-set)))
     (if (not set-point) (message "WARNING: No open sets.")
       (let ((pom-point (org-tomato--find-last-pomodoro set-point)))
         (if (not pom-point) (message "WARNING: No pomodoros.")
           (let ((cycle (org-tomato--get-pomodoro-cycle pom-point))
                 (open-clock (org-tomato--get-open-clock pom-point)))
             (if (not open-clock) (message "WARNING: No open clocks.")
               (org-tomato--finish-clock pom-point))
             (if (= cycle org-tomato-cycles)
                 (progn
                   (org-tomato--close-set set-point)
                   (org-tomato--start-rest-timer t))
               (org-tomato--start-rest-timer)))))))))

(defun org-tomato--kill ()
  "Close clocks and timers and close current set."
  (org-tomato--cancel-timer)
  (org-tomato--with-log-file-save
   (let ((set-point (org-tomato--find-last-set)))
     (if (not set-point) (message "WARNING: No open sets.")
       (progn
         (org-tomato--close-set set-point)
         (let ((pom-point (org-tomato--find-last-pomodoro set-point)))
           (if (not pom-point) (message "WARNING: No pomodoros.")
             (let ((open-clock (org-tomato--get-open-clock pom-point)))
               (if (not open-clock) (message "WARNING: No open clocks.")
                 (org-tomato--finish-clock pom-point))))))))))

;; interactive signaling functions

(defun org-tomato-user-clock-in ()
  "Send a clock-in signal to the org-tomato state machine."
  (interactive)
  (setq org-tomato--state
        (org-tomato--next-state
         :clock-in org-tomato--state)))

(defun org-tomato-user-clock-out ()
  "Send a clock-out signal to the org-tomato state machine."
  (interactive)
  (setq org-tomato--state
        (org-tomato--next-state
         :clock-out org-tomato--state)))

(defun org-tomato-user-kill ()
  "Send a kill signal to the org-tomato state machine."
  (interactive)
  (setq org-tomato--state
        (org-tomato--next-state
         :kill org-tomato--state)))

(defun org-tomato-user-get-summary ()
  "Display a summary of the current state, including timer and cycle."
  (interactive)
  (let* ((state (org-tomato--state-name org-tomato--state))
         (cycle (org-tomato--with-log-file
                 (-some-> (org-tomato--find-last-set)
                          org-tomato--find-last-pomodoro
                          org-tomato--get-pomodoro-cycle)))
         (format-time
          (lambda (time)
            (let ((min (abs (/ time 60)))
                  (sec (abs (% time 60))))
              (--> (format "%d:%02d" min sec)
                   (if (< 0 time) it (concat "-" it))))))
         (timer (-some--> org-tomato--timer
                          (timer--time it)
                          (float-time it)
                          (- it (float-time))
                          (round it)
                          (funcall format-time it))))
    (cond
     ((eq 'inactive state)
      (message "Current state: %s" state))
     (t
      (message "Current state: %s on cycle %s with %s remaining"
               state
               cycle
               timer)))))

(defun org-tomato-user-pomodoro-goto ()
  "Open the log file and go to the last pomodoro."
  (interactive)
  (org-tomato--with-log-file
   (let ((set-point (org-tomato--find-last-set)))
     (if (not set-point)
         (message "WARNING: No sets available. Nothing to do.")
       (progn
         (-> (save-excursion (goto-char set-point) (point-marker))
             marker-buffer
             pop-to-buffer-same-window)
         (let ((pom-point (org-tomato--find-last-pomodoro set-point)))
           (if (not pom-point)
               (progn
                 (message "WARNING: No pomodoros available. Going to last set.")
                 (goto-char set-point))
             (goto-char pom-point)))
         ;; (if (or (< m (point-min)) (> m (point-max))) (widen))
         (org-show-entry)
         (org-back-to-heading t)
         (org-cycle-hide-drawers 'children)
         (recenter org-clock-goto-before-context)
         (org-reveal))))))

(provide 'org-tomato)
;;; org-tomato.el ends here
