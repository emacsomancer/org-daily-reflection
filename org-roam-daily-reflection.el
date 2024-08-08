;;; org-roam-daily-reflection.el --- concurrent display of org-roam dailies  -*- lexical-binding: t; -*-

;; Org Roam Daily Reflection - compare N org-roam dailies at M intervals 

;; Copyright (C) 2024 Benjamin Slade

;; Author: Benjamin Slade <slade@lambda-y.net>
;; Maintainer: Benjamin Slade <slade@lambda-y.net>
;; URL: https://github.com/emacsomancer/org-roam-daily-reflection
;; Package-Version: 0.031
;; Version: 0.031
;; Package-Requires: ((emacs "26.1") (org "9.4"))
;; Created: 2024-07-27
;; Keywords: convenience, frames, terminals, tools, window-system

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Reflect on your org-roam daily journal entries. Shows a range of
;; split windows, each with an org-roam daily in them, at specified intervals. 

;; You can use like one does with a paper three- or five-year diary,
;; to see what was happening on this day last year, and the year prior, etc.

;; But you could also look at the last six days of daily journals, or dailies
;; at other specified intervals (e.g. days, weeks, fortnights, months,
;; years, decades, centuries).

;;; Installation:
;; Not yet on Melpa. See README.org for suggestions.

;;; Usage:
;; Interactive interface with the function `org-roam-daily-reflect', which
;; can also be passed a span and range number directly in elisp 
;; (e.g. 'year and 3, to get dailies comparing this day on
;; the last three years.) A few sample one-shot functions with prefix
;; `org-roam-reflect-on-last-' are included.

;;; Advice:
;; None currently. ... 

;;; Code:
(eval-when-compile (require 'cl-lib) ;; for cl-loops
                   (require 'org)) 

;; doesn't actually require org-roam,
;; although currently it has more functionality if org-roam is enabled
;; (require 'org-roam) 

(defgroup org-roam-daily-reflection ()
  "Compare N org-roam dailies at M intervals."
  :group 'org)

(defcustom org-roam-daily-reflection-disable-org-roam-daily-check nil
  "Whether or not to check for current buffer actually being an org-roam
daily or not. (Potentially useful in symlinked or other non-standard
file-system cases.)"
  :group 'org-roam-daily-reflection
  :type 'boolean)

(defcustom org-roam-daily-reflection-direction-of-window-splits 'auto
  "Determining direction of window splits. Choices are 'auto, 'horizontal
or 'vertical. 'auto tries to calculate the optimal direction of split."
  :group 'org-roam-daily-reflection
  :type 'symbol)

(defcustom org-roam-daily-reflection-time-spans '(day week fortnight month year)
  "Possible time-spans, including days, weeks, fortnights, months, and years,
by default. You can also add `decade' and/or `century' to this list if you
use these spans."
  :group 'org-roam-daily-reflection
  :type 'sexp)

(defcustom org-roam-daily-reflection-leap-day-is-special t 
  "If `t', then when the time-span is `year' and the current entry is a leap-day,
then show the previous `n' leap-days (so with 4-year gaps, rather than 1-year).
If `nil', then just show 365 days before."
  :group 'org-roam-daily-reflection
  :type 'boolean)

(defcustom org-roam-daily-reflection-dailies-directory
  (when (boundp 'org-roam-dailies-directory)
    (file-truename org-roam-dailies-directory))
  "Path to daily-notes."
  :group 'org-roam-daily-reflection
  :type 'string)

(defcustom org-roam-daily-reflection-capture-keys nil
  "Template to use for opening dailies. E.g., \"d\" for default.
(Otherwise, if you have multiple org-roam capture templates,
you will be asked for each window to interactively choose a template.)"
  :group 'org-roam-daily-reflection
  :type 'string)

(defcustom org-roam-daily-reflection-capture-nascent-files t
  "Run org-roam capture functions & hooks (if available) for currently
non-extant daily journals."
  :group 'org-roam-daily-reflection
  :type 'boolean)

;;; interactive wrapper function
;;;###autoload
(defun org-roam-daily-reflect (&optional m n)
  "Show `n' number of `m' time spans of org-roam dailies from the current
org-mode daily. choices for `n' are integers and choices for `m' are `day',
`week', `fortnight', `month', `year', `decade', and `century'."
  (interactive)

  ;; directory check
  (unless org-roam-daily-reflection-dailies-directory
    (user-error "You need to set `org-roam-daily-reflection-dailies-directory'
before running. (It seems you likely don't have `org-roam-dailies-directory' set.)"))
  
  ;; org-roam-daily-reflect directory/file check
  (unless (or
           (org-roam-daily-reflect--daily-note-p)
           org-roam-daily-reflection-disable-org-roam-daily-check)
    (user-error "Not in a daily-note."))
    
  ;; ask user for `m' and `n' if called interactively
  (let* ((m (or m
                (when (called-interactively-p 'any)
                  (intern
                   (message "%s"
                            (completing-read "What time interval?"
                                             org-roam-daily-reflection-time-spans
                                             nil t nil nil))))
                'year))
         (n (or n
                (when (called-interactively-p 'any)
                  (1- ;; restore old interactive behaviour, as maybe more intuitive
                   (string-to-number
                    (message "%s"
                             (read-number
                              (concat "How many "
                                      (symbol-name m) "s? "))))))
                3)))
    ;; put `m' and `n' into a cons cell and wrap that in a list
    ;; and call the main function
    (org-roam-daily-reflection--reflect (list (cons m n)))))

;;; main function (called by interactive version)
(defun org-roam-daily-reflection--reflect (date-list)
  "Accepts an alist of time-spans and intervals, e.g., 
(('day . 3) ('month 4) ('year . 2)), and splits up
the frame into the appropriate number of windows and
shows the result in chronological order.

E.g., given the above example input, the function would
show the 3 days preceding the current journal entry,
preceded by dailies for 4 months before that, preceded by
dailies for 2 years before the earliest of the monthly
dailies."
  (let (;; count how many windows are needed total
        (total-splits
          (cl-loop for (key . value) in date-list
                   sum value))
        ;;; Unix time issue flag and escape
        (org-roam-daily-reflection--unix-time-issue-maybe nil)
        (org-read-date-force-compatible-dates nil)

        ;; set the initial date point
        (start-daily
         (file-name-base buffer-file-name))
        ;; initialise collected dates
        (collected-dates nil))

    ;; First, create the needed splits (if possible)
    (org-roam-reflect--determine-splits total-splits)

    ;; work through the list of different spans in order
    (cl-loop for (key . value) in date-list
             with oldest-seen-daily = nil ;; reset at beginning of loop
             do (let ((m key)
                      (n value))

                  ;; if we're not the first time in the loop
                  ;; set the new date entry point as the
                  ;; earliest date we've seen
                  (when oldest-seen-daily
                    (setq start-daily oldest-seen-daily))

                  ;; Then, start from the furthest back `m' (`n' `m's ago) date
                  ;; within each list of time spans, 
                  ;; and successively find+open the journal entry for those dailies:
                  (cl-loop for i from n downto 1
                           with inner-collected-dates = nil 
                           do (let* ((last-seen-daily
                                      (org-roam-reflect--determine-prev-journal-entry
                                       (org-read-date nil nil start-daily)
                                       i ;; start with greatest number for furthest back
                                       m)))

                                ;; we're collecting up the list of dates we've calculated
                                ;; (add-to-list 'inner-collected-dates last-seen-daily t)
                                (push last-seen-daily inner-collected-dates)
                                
                                ;; Check for Unix time issue for `last-seen-daily'
                                (let ((internal-emacs-time
                                       (org-time-string-to-time last-seen-daily)))
                                  (when
                                      (or
                                       (time-less-p internal-emacs-time
                                                    (org-time-string-to-time "1970-01-01"))
                                       (time-less-p
                                        (org-time-string-to-time "2038-01-01")
                                        internal-emacs-time))
                                    (setq org-roam-daily-reflection--unix-time-issue-maybe t))))
                           finally
                           (progn
                             ;; at the end, set the `oldest-seen-daily' to the
                             ;; front of the inner collected list
                             (let ((inner-collected-dates-chronologically
                                    (nreverse inner-collected-dates)))
                               (setq oldest-seen-daily (car inner-collected-dates-chronologically))
                               ;;  and add the list of collected dates to the outer collected dates list
                               (add-to-list 'collected-dates inner-collected-dates-chronologically))))))

    ;; Open returned (potential) org-roam daily journal entries:
    (mapc #'org-roam-reflect--open-prev-journal-entry
          (apply #'append collected-dates))  ;; flatten the list of lists of dates
        
    ;; Finally, return the point to the top of the first buffer.
    (goto-char (point-min))

    ;; and then, do post-hoc checking for Unix time issue, and warn user.
    (when org-roam-daily-reflection--unix-time-issue-maybe
      (message "Warning: dates before 1970-1-1 or after 2038-1-1 cannot always
be represented correctly. See docstring for `org-read-date-force-compatible-dates'
for more information."))))

;;; note-check
(defun org-roam-daily-reflect--daily-note-p ()
  "Return t if current-buffer is an org-mode file in
`org-roam-daily-reflection-dailies-directory' (which is set by default
to `org-roam-dailies-directory' if available), nil otherwise."
  (when-let ((a (expand-file-name
                 (buffer-file-name (buffer-base-buffer))))
             (b (expand-file-name
                 org-roam-daily-reflection-dailies-directory)))
    (setq a (expand-file-name a))
    (if (and (eq major-mode 'org-mode)
             (unless (and a b (equal (file-truename a) (file-truename b)))
               (string-prefix-p (replace-regexp-in-string "^\\([A-Za-z]\\):" 'downcase
                                                          (expand-file-name b) t t)
                                (replace-regexp-in-string "^\\([A-Za-z]\\):" 'downcase
                                                          (expand-file-name a) t t))))
        t nil)))


(defun org-roam-reflect--determine-splits (no-of-splits)
  "Split the frame into `no-of-splits' number of windows in the
appropriate configuration."
  (let ((split-direction
         org-roam-daily-reflection-direction-of-window-splits))
    (when (equal split-direction 'auto)
      (let ((width (frame-width))
            (height (frame-height)))
        ;; if screen is less than 80 pixes in width, or the
        ;; display is longer than it is wide, then set
        ;; split to horizontal, otherwise vertical.
        (if (or (< width 80)   
                (< (/ width height) 1)) 
            (setq split-direction 'horizontal)
          (setq split-direction 'vertical))))
    
    ;; *Try* to split window `no-of-splits' less one times:
    (let ((reflect-split (if (equal split-direction 'vertical)
                             #'split-window-right
                           #'split-window-below)))
      
      ;; first, save current window config
      (window-configuration-to-register 'org-roam-daily-reflect)
      
      (unless ;; restore window configuration and notify user if error in splitting
          (ignore-errors ;; return nil if error
            (delete-other-windows)
            ;; (cl-loop for i from (1- no-of-splits) downto 1
            (cl-loop for i from no-of-splits downto 1             
                     do
                     (progn
                       (funcall reflect-split)
                       (balance-windows)))
            t) ;; if successful
        (jump-to-register 'org-roam-daily-reflect) ;; restore old window config,
        ;; if split failed
        (error (concat "Frame too small to show "
                       (number-to-string no-of-splits)
                       " windows."))))))

(defun org-roam-reflect--prev-node-extant-file (org-date)
  "Determines whether an org-roam daily already exists for
`org-date' date; return its path if it does."
  (let ((potential-file
         (concat (expand-file-name org-roam-daily-reflection-dailies-directory)
                 "/" org-date ".org")))
    (when (file-exists-p potential-file)
      potential-file)))

(defun org-roam-reflect--determine-prev-journal-entry (org-curr-date offset unit)
  "Find the appropriate org-roam daily journal that is `offset' number of 
`units' before `org-curr-date'."
  (let* ((ocd-date (decode-time (org-time-string-to-time org-curr-date)))
         (ocd-year (nth 5 ocd-date))
         (ocd-month (nth 4 ocd-date))
         (ocd-day (nth 3 ocd-date))
         ;; set locally to allow proper finding of potential entries:
         (org-read-date-force-compatible-dates nil))
    
    (cond
     ((equal unit 'century)
      (let ((back-by (* offset 100)))
        ;; select the journal entry `back-by' centuries (100 years) prior
        (org-read-date nil nil
                       (concat "--" (number-to-string back-by) "y")
                       nil (org-time-string-to-time org-curr-date))))
     
     ((equal unit 'decade)
      (let ((back-by (* offset 10)))
        ;; select the journal entry `back-by' decades (10 years) prior
        (org-read-date nil nil
                       (concat "--" (number-to-string back-by) "y")
                       nil (org-time-string-to-time org-curr-date))))
     
     ((equal unit 'year)
      ;; if it's a leap day, then go back 4 years to prev leap day
      ;; unless special leap-day functionality is disabled
      (let ((back-by
             (if (and org-roam-daily-reflection-leap-day-is-special
                      (date-leap-year-p ocd-year)
                      (= ocd-month 02)
                      (= ocd-day 29))
                 (* offset 4)
               ;; if not a leap day, then just 1 year
               offset)))
        ;; select the journal entry `back-by' years prior
        (org-read-date nil nil
                       (concat "--" (number-to-string back-by) "y")
                       nil
                       (org-time-string-to-time org-curr-date))))
     
     ;; for the remaining units, go back 30 days,
     ;;  2 weeks, 1 week, or 1 day, respectively.

     ((equal unit 'month)
      (let ((back-by (* offset 30)))
        ;; select the journal entry `back-by' months (30 days) prior
        (org-read-date nil nil
                       (concat "--" (number-to-string back-by) "d")
                       nil (org-time-string-to-time org-curr-date))))
     
     ((equal unit 'fortnight)
      (let ((back-by (* offset 2)))
        ;; select the journal entry `back-by' fortnights prior
        (org-read-date nil nil
                       (concat "--" (number-to-string back-by) "w")
                       nil (org-time-string-to-time org-curr-date))))
     
     ((equal unit 'week)
      (let ((back-by offset))
        ;; select the journal entry `back-by' weeks prior
        (org-read-date nil nil
                       (concat "--" (number-to-string back-by) "w")
                       nil (org-time-string-to-time org-curr-date))))

     ((equal unit 'day)
      (let ((back-by offset))
        ;; select the journal entry `back-by' days prior
        (org-read-date nil nil
                       (concat "--" (number-to-string back-by) "d")
                       nil (org-time-string-to-time org-curr-date))))
     (t (user-error "Unrecognised unit.")))))

(defun org-roam-reflect--open-prev-journal-entry (earlier-journal-entry)
  "Open the appropriate org-roam daily journal that is `offset' number of 
`units' before `org-curr-date'."

  (let (;; set locally to allow proper opening of potential entries:
        (org-read-date-force-compatible-dates nil))

    ;; If it is available, then run `org-roam-dailies--capture'
    ;; with non-nil GOTO optional arg (this seems the best way
    ;; to open both existing and nascent org-roam files)
    ;; and go to the note without creating an entry;
    ;; this creates a daily-note for TIME (first arg) if necessary.
    (if (and (fboundp 'org-roam-dailies--capture)
             org-roam-daily-reflection-capture-nascent-files)
        ;; run org-roam-daily capture function
        (org-roam-dailies--capture
         (org-read-date nil t
                        earlier-journal-entry
                        nil)
         t
         org-roam-daily-reflection-capture-keys)
      
      ;; Else, if not using Org-roam, for now just try opening the file, if extant
      ;; and post a message that no entry exists otherwise.
      (if (org-roam-reflect--prev-node-extant-file earlier-journal-entry)
          (find-file (concat
                      (expand-file-name org-roam-daily-reflection-dailies-directory)
                      "/" earlier-journal-entry ".org"))

        ;; make an invisible buffer and use pages to display missing entry messages:
        (switch-to-buffer
         (generate-new-buffer-name " *Org Roam Daily Reflection Absences*"))
        (goto-char (point-max))
        (insert (concat "\n\f\n* No daily journal entry for " earlier-journal-entry ".\n"))
        (narrow-to-page)
        (visual-line-mode)
        (goto-char (point-min))))

    ;; Mark non-previously existing daily buffers as unmodified
    ;; (this prevents littering `buffers' with spurious would-be files).
    (unless (org-roam-reflect--prev-node-extant-file earlier-journal-entry)
      (set-buffer-modified-p nil)))

  ;; move the window focus to the next window (right or down).
  (other-window 1))

;;; various predefined reflection commands

;;;###autoload
(defun org-roam-reflect-on-last-three-years ()
  "Compare the daily for the current day to the same day on the
previous two years."
  (interactive)
  (org-roam-daily-reflect 'year 3))

;;;###autoload
(defun org-roam-reflect-on-last-three-months ()
  "Compare the daily for the last three months."
  (interactive)
  (org-roam-daily-reflect 'month 3))

;;;###autoload
(defun org-roam-reflect-on-last-two-fortnights ()
  "Compare the daily for today and the preceding fortnight."
  (interactive)
  (org-roam-daily-reflect 'fortnight 2))

;;;###autoload
(defun org-roam-reflect-on-last-four-weeks ()
  "Compare the daily for the last four weeks."
  (interactive)
  (org-roam-daily-reflect 'week 4))

;;;###autoload
(defun org-roam-reflect-on-last-five-decades ()
  "Compare the daily for the last half century."
  (interactive)
  (org-roam-daily-reflect 'decade 5))

;;;###autoload
(defun org-roam-reflect-on-last-five-days ()
  "Compare the daily for the last five days."
  (interactive)
  (org-roam-daily-reflect 'day 5))

(provide 'org-roam-daily-reflection)

;;; org-roam-daily-reflection.el ends here
