;;; org-tomato.el --- Org-Mode Pomodoro Implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Nathan Dwarshuis

;; Author: Nathan Dwarshuis <natedwarshuis@gmail.com>
;; Keywords: org-mode
;; Homepage: https://github.com/ndwarshuis/org-tomato
;; Package-Requires: ((emacs "25") (dash "2.15") (sound-wav "0.02"))
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

;; This is a simple implementation of the pomodoro technique for org
;; mode. It uses a simple state machine to keep track of pomodoros and
;; breaks and also writes pomodoros into a log file to track progress.

;; It also has convenient functions to clock in/out of pomodoros as
;; well as timers and notifications to alert the user when pomodoros
;; and breaks are finished.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dash)
(require 'sound-wav)
(require 'org)
(require 'org-clock)
(require 'org-agenda)

;;; custom variables

(defgroup org-tomato nil
  "Org Tomato Options."
  :tag "Org-Tomato"
  :group 'org)

(defcustom org-tomato-log-file "~/Org/pomodoro.org_archive"
  "The file in which org pomodoros are logged.
If logging is not desired, set this to an in-memory file path."
  :type 'file
  :group 'org-tomato)

(defcustom org-tomato-pomodoro-length 1500
  "Time length of a pomodoro in seconds."
  :type 'file
  :group 'org-tomato)

(defcustom org-tomato-break-length 300
  "Time length of a short break in seconds."
  :type 'integer
  :group 'org-tomato)

(defcustom org-tomato-long-break-length 1200
  "Time length of a long break in seconds."
  :type 'integer
  :group 'org-tomato)

(defcustom org-tomato-log-rotate-interval 30
  "The time spanned in days before log files are rotated."
  :type 'integer
  :group 'org-tomato)

(defcustom org-tomato-timer-sound nil
  "The file to play when any timer reaches zero. Must be a WAV file.
Set to nil to disable timer sounds."
  :type 'file
  :group 'org-tomato)

(defcustom org-tomato-cycles 4
  "The number of pomodoros that complete a full set.
The original technique has this as 4 (the default)."
  :type 'integer
  :group 'org-tomato)

;;; internal variables

(defvar org-tomato--timer nil
  "The timer object for org tomato.")

(defvar org-tomato--state nil
  "The current state of the org tomato state machine.")

(defvar org-tomato--transition-table nil
  "Org tomato state machine transition table.")

;;; advising hacks

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

(defun org-tomato--resolve-clocks-ad (orig-sub &rest args)
  "Add advice to ignore a list of files when resolving clocks.
ORIG-SUB is the subroutine to be advised and ARGS are its arguments."
  (org-tomato--with-advice
      ((#'org-files-list
        :around
        (lambda (f)
          (-remove-item
           (expand-file-name org-tomato-log-file)
           (funcall f)))))
    (unwind-protect (apply orig-sub args))))

(advice-add #'org-resolve-clocks :around #'org-tomato--resolve-clocks-ad)

;;; log file context abstractions

(defun org-tomato--log-insert-header ()
  "Insert the heading into `org-tomato-log-file'."
  (insert
   (string-join
    (list "#    -*- mode: org -*-\n"
          "\n"
          (format "# Created: %s\n" (format-time-string "%Y-%m-%d"))
          "\n"))))

(defun org-tomato--log-rotate ()
  "Rotate `org-tomato-log-file' if older than `org-tomato-log-rotate-interval'."
  (save-excursion
    (goto-char (point-min))
    (let* ((created-time
            (save-match-data
              (re-search-forward
               "# Created: \\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\)$"
               nil t)
              (match-string 1)))
           (diff (->> created-time
                      parse-time-string
                      (-replace nil 0)
                      (apply #'encode-time)
                      float-time
                      round
                      (- (round (float-time))))))
      (when (> diff (* 24 60 60 org-tomato-log-rotate-interval))
        ;; if there are no sets in the log file, just erase everything
        ;; and insert the new date
        (when (org-tomato--log-find-last-set)
          (let* ((ext (file-name-extension org-tomato-log-file))
                 (base (file-name-sans-extension org-tomato-log-file))
                 (new (format "%s_%s.%s" base created-time ext)))
            (copy-file org-tomato-log-file new t))
          (erase-buffer)
          (org-tomato--log-insert-header))))))

(defmacro org-tomato--log-with-file (&rest body)
  "Open `org-tomato-log-file' in execute BODY."
  `(let ((buffer (find-file-noselect org-tomato-log-file)))
     (with-current-buffer buffer ,@body)))

(defmacro org-tomato--log-goto (point &rest body)
  "Go to POINT in `org-tomato-log-file' and execute BODY."
  `(org-tomato--log-with-file
    (save-excursion (goto-char ,point) ,@body)))

(defmacro org-tomato--log-goto-save (point &rest body)
  "Go to POINT in `org-tomato-log-file' and execute BODY.
Save the buffer when finished."
  `(org-tomato--log-with-file
    (when (not (file-exists-p org-tomato-log-file))
        (org-tomato--log-insert-header))
    (org-tomato--log-goto ,point ,@body)
    (let ((inhibit-message t)) (save-buffer))))

(defmacro org-tomato--log-goto-save-rotate (point &rest body)
  "Go to POINT in `org-tomato-log-file' and execute BODY.
Save the buffer when finished and rotate the log files if required."
  `(org-tomato--log-with-file
    (if (not (file-exists-p org-tomato-log-file))
        (org-tomato--log-insert-header)
      (org-tomato--log-rotate))
    (org-tomato--log-goto ,point ,@body)
    (let ((inhibit-message t)) (save-buffer))))

;;; timers

(defun org-tomato--notify (message)
  "Notify the user with MESSAGE."
  (org-notify message)
  (if (file-exists-p org-tomato-timer-sound)
      (sound-wav-play org-tomato-timer-sound)
    (message "WARNING: %s not found." org-tomato-timer-sound)))

(defun org-tomato--timer-start-pomodoro (&optional diff)
  "Start a pomodoro timer.
DIFF is amount of time to subtract from `org-tomato-pomodoro-length'."
  (let ((length (- org-tomato-pomodoro-length (or diff 0))))
    (org-tomato--timer-set length "Pomodoro Ended")))

(defun org-tomato--timer-start-rest (&optional long)
  "Start a break timer.
Use `org-tomato-long-break-length' when LONG, otherwise use
`org-tomato-break-length'."
  (if (not long)
      (org-tomato--timer-set org-tomato-break-length "Break Ended")
    (org-tomato--timer-set org-tomato-long-break-length "Long Break Ended")))

(defun org-tomato--timer-cancel ()
  "Cancel the currently running timer."
  (when (timerp org-tomato--timer)
    (cancel-timer org-tomato--timer))
  (setq org-tomato--timer nil))

(defun org-tomato--timer-set (future message)
  "Set a new timer FUTURE seconds into the future with MESSAGE."
  (org-tomato--timer-cancel)
  (setq org-tomato--timer
        (run-at-time future nil #'org-tomato--notify message)))

;; log file operations

(defun org-tomato--log-read-past-seconds (past-clock)
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

(defun org-tomato--log-set-is-closed-p (point)
  "Test if the set under POINT in `org-tomato-log-file' is closed."
  (org-tomato--log-goto
   point
   (let ((not-top (org-up-heading-safe)))
     (while not-top (setq not-top (org-up-heading-safe))))
   (equal "DONE" (org-get-todo-state))))

(defun org-tomato--log-sum-pomodoro-clock (point)
  "Return clock sum under headline at POINT in `org-tomato-log-file'.
Return an integer in seconds or nil if no clocks are found."
  (org-tomato--log-goto
   point
   (let ((s (point))
         (e (save-excursion (outline-next-heading) (point)))
         (time-convert
          (lambda (hhmm)
            (--> (split-string hhmm ":")
                 (mapcar #'string-to-number it)
                 (-> (car it) (* 60) (+ (cadr it)) (* 60))))))
     (-->
      (org-element--parse-elements s e 'first-section nil nil nil nil)
      (car it)
      (org-element-map it 'clock (lambda (c) (org-element-property :duration c)))
      (--map (funcall time-convert it) it)
      (cl-reduce '+ it)))))

(defun org-tomato--log-get-pomodoro-cycle (point)
  "Return cycle number of pomodoro at POINT in `org-tomato-log-file'.
Return nil if POINT is not on a pomodoro headline."
  (org-tomato--log-goto
   point
   (beginning-of-line)
   (save-match-data
     (when (looking-at "\\*\\* pomodoro \\([[:digit:]]\\)")
       (string-to-number (match-string 1))))))

(defun org-tomato--log-get-open-clock (point)
  "Return open clock value under POINT in `org-tomato-log-file'.
Return nil if no open clock found or an integer in seconds (unixtime)."
  (org-tomato--log-goto
   point
   (save-match-data
     (let ((end (save-excursion (outline-next-heading) (point))))
       (when (re-search-forward
              (concat "[ \t]*" org-keyword-time-regexp)
              end t)
         (-some-->
          (org-element-context)
          (org-element-property :value it)
          (and (eq 'inactive (org-element-property :type it)) it)
          (org-element-property :raw-value it)
          (org-2ft it)
          (round it)))))))

(defun org-tomato--log-start-clock (point)
  "Start clock at headline under POINT in `org-tomato-log-file'."
  (org-tomato--log-goto-save
   point
   (org-clock-find-position nil)
   (insert-before-markers "\n")
   (backward-char 1)
   (org-indent-line)
   (->> (float-time)
        (format-time-string (org-time-stamp-format t t))
        (concat org-clock-string " ")
        (insert))))

(defun org-tomato--log-finish-clock (point &optional elapsed)
  "Close clock for the headline under POINT in `org-tomato-log-file'.
ELAPSED is the length of the clock in minutes; if given, use this to
set the final clock time instead of the current time."
  (org-tomato--log-goto-save
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

(defun org-tomato--log-find-last-pomodoro (point)
  "Return point of latest pomodoro in set under POINT in `org-tomato-log-file'."
  (org-tomato--log-goto
   point
   (org-end-of-subtree)
   (search-backward-regexp "\\*\\* pomodoro [[:digit:]]" point t)))

(defun org-tomato--log-find-last-set ()
  "Return point of last set in `org-tomato-log-file' or nil if none."
  (org-tomato--log-goto
   (point-max)
   (search-backward-regexp "\\* \\(TODO\\|DONE\\) set" nil t)))

(defun org-tomato--log-insert-pomodoro (n)
  "Insert a new pomodoro headline at the end of `org-tomato-log-file'.
N is the current pomodoro cycle."
  (org-tomato--log-goto-save
   (point-max)
   (unless (= 10 (char-before)) (end-of-line) (insert "\n"))
   (insert (format "** pomodoro %s\n" n))))

(defun org-tomato--log-insert-set ()
  "Insert a new set headline at the end of `org-tomato-log-file'."
  (org-tomato--log-goto-save
   (point-max)
   (unless (= 10 (char-before)) (end-of-line) (insert "\n"))
   (insert "* TODO set\n")))

(defun org-tomato--log-close-set (point)
  "Close the headline under POINT in `org-tomato-log-file'."
  ;; this is the only point when rotating a log file is deemed "safe"
  (org-tomato--log-goto-save-rotate
   point
   (let ((org-todo-log-states nil))
     (org-todo 'done))))

;; state machine hooks

(defun org-tomato--message (state)
  "Send a message for new STATE."
  (message "New Org-Tomato state: %s" state))

(defun org-tomato--clock-in ()
  "Clock into a pomodoro given old and new states O and N."
  (org-tomato--timer-cancel)
  (let ((set-point (org-tomato--log-find-last-set)))
    ;; insert new set if there are no sets or no open sets
    (when (or (not set-point) (org-tomato--log-set-is-closed-p set-point))
      (org-tomato--log-insert-set)
      (setq set-point (org-tomato--log-find-last-set)))

    ;; insert pomodoro at cycle 1 if there are no pomodoros in set
    (let ((pom-point (org-tomato--log-find-last-pomodoro set-point)))
      (if (not pom-point)
          (org-tomato--log-insert-pomodoro 1)

        ;; else insert pomodoros/sets based on what is in current set
        (let ((cycle (org-tomato--log-get-pomodoro-cycle pom-point))
              (open-clock (org-tomato--log-get-open-clock pom-point)))

          ;; if open clock, prompt user for how much of the elapsed
          ;; time to keep
          (-some->> open-clock
                    (org-tomato--log-read-past-seconds)
                    (org-tomato--log-finish-clock pom-point))

          ;; determine if we need to insert new sets or pomodoros
          (cond
           ;; insert next pomodoro if no open clocks and set not full
           ((and (not open-clock) (< cycle org-tomato-cycles))
            (org-tomato--log-insert-pomodoro (+ 1 cycle)))

           ;; possibly insert next pomodoro if clock is open, set
           ;; is not full, and we want to use the old set. Do nothing
           ;; if we want to use the last pomodoro
           ((and open-clock
                 (< cycle org-tomato-cycles)
                 (y-or-n-p "Open set detected. Use old set? "))
            (unless (y-or-n-p "Continue with last pomodoro? ")
              (org-tomato--log-insert-pomodoro (+ 1 cycle))))

           ;; all other cases, close last set and insert new set
           (t
            (org-tomato--log-close-set set-point)
            (org-tomato--log-insert-set)
            (org-tomato--log-insert-pomodoro 1))))))

    ;; after placing all sets/pomodoros, start clock and timer
    (-> (org-tomato--log-find-last-set)
        org-tomato--log-find-last-pomodoro
        org-tomato--log-start-clock)
    (-> (org-tomato--log-find-last-set)
        org-tomato--log-find-last-pomodoro
        org-tomato--log-sum-pomodoro-clock
        org-tomato--timer-start-pomodoro)))

(defun org-tomato--clock-out ()
  "Clock out a pomodoro and close the set if complete."
  (org-tomato--timer-cancel)
  (let ((set-point (org-tomato--log-find-last-set)))
    (if (not set-point) (message "WARNING: No open sets.")
      (let ((pom-point (org-tomato--log-find-last-pomodoro set-point)))
        (if (not pom-point) (message "WARNING: No pomodoros.")
          (let ((cycle (org-tomato--log-get-pomodoro-cycle pom-point))
                (open-clock (org-tomato--log-get-open-clock pom-point)))
            (if (not open-clock) (message "WARNING: No open clocks.")
              (org-tomato--log-finish-clock pom-point))
            (if (= cycle org-tomato-cycles)
                (progn
                  (org-tomato--log-close-set set-point)
                  (org-tomato--timer-start-rest t))
              (org-tomato--timer-start-rest))))))))

(defun org-tomato--kill ()
  "Close clocks and timers and close current set."
  (org-tomato--timer-cancel)
  (let ((set-point (org-tomato--log-find-last-set)))
    (if (not set-point) (message "WARNING: No open sets.")
      (progn
        (org-tomato--log-close-set set-point)
        (let ((pom-point (org-tomato--log-find-last-pomodoro set-point)))
          (if (not pom-point) (message "WARNING: No pomodoros.")
            (let ((open-clock (org-tomato--log-get-open-clock pom-point)))
              (if (not open-clock) (message "WARNING: No open clocks.")
                (org-tomato--log-finish-clock pom-point)))))))))

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

(defun org-tomato--create-transition (old new sig &rest hooks)
  "Create a state machine transition.
OLD and NEW are the names of the old and new state. SIG is the signal
to trigger the transition. HOOKS are subroutines to be run with no
arguments when the transition is triggered."
  (org-tomato--transition-create
   :old-state old
   :new-state new
   :signal sig
   :hooks `(,@hooks)))

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
       (let* ((new-state-name (org-tomato--transition-new-state trans)))
         ;; perform side effects before returning new state
         ;; this guarentees that everything is ok before finalizing
         ;; the new state
         (-> trans org-tomato--transition-hooks (--each (funcall it)))
         (org-tomato--state-create :name new-state-name
                                   :timestamp (float-time))))))

;; initialization

(setq org-tomato--state (org-tomato--state-create :name 'inactive))

;; interactive functions

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

(defun org-tomato-user-hl-clock-in ()
  "Clock into an Org headline and pomodoro simultaneously."
  (interactive)
  (org-clock-in)
  (org-tomato-user-clock-in))

(defun org-tomato-user-hl-clock-out ()
  "Clock out of an Org headline and pomodoro simultaneously."
  (interactive)
  (org-clock-out)
  (org-tomato-user-clock-out))

(defun org-tomato-user-hl-agenda-clock-in ()
  "Clock into an Org headline and pomodoro simultaneously."
  (interactive)
  (org-agenda-clock-in)
  (org-tomato-user-clock-in))

(defun org-tomato-user-hl-agenda-clock-out ()
  "Clock out of an Org headline and pomodoro simultaneously."
  (interactive)
  (org-agenda-clock-out)
  (org-tomato-user-clock-out))

(defun org-tomato-user-get-summary ()
  "Display a summary of the current state, including timer and cycle."
  (interactive)
  (let* ((state (org-tomato--state-name org-tomato--state))
         (cycle (-some-> (org-tomato--log-find-last-set)
                         org-tomato--log-find-last-pomodoro
                         org-tomato--log-get-pomodoro-cycle))
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
  (org-tomato--log-with-file
   (let ((set-point (org-tomato--log-find-last-set)))
     (if (not set-point)
         (message "WARNING: No sets available. Nothing to do.")
       (progn
         (-> (save-excursion (goto-char set-point) (point-marker))
             marker-buffer
             pop-to-buffer-same-window)
         (let ((pom-point (org-tomato--log-find-last-pomodoro set-point)))
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
