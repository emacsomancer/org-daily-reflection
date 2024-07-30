;;; org-roam-daily-reflection.el --- concurrent display of org-roam dailies  -*- lexical-binding: t; -*-

;; Org Roam Daily Reflection - compare N org-roam dailies at M intervals 

;; Copyright (C) 2024 Benjamin Slade

;; Author: Benjamin Slade <slade@lambda-y.net>
;; Maintainer: Benjamin Slade <slade@lambda-y.net>
;; URL: https://github.com/emacsomancer/org-roam-daily-reflection
;; Package-Version: 0.01
;; Version: 0.01
;; Package-Requires: ((emacs "26.1") (org "9.4") (org-roam "2.1"))
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
(eval-when-compile (require 'cl-lib)) ;; for cl-loops

(require 'org)
(require 'org-roam)

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

(defcustom org-roam-daily-reflection-time-spans '("day" "week" "fortnight" "month" "year" "decade" "century")
  "Possible time-spans, including days, weeks, fortnights, months, years, and decades.
(Currently pointless/dangerous to change as the package won't know what to do with
other spans.)"
  :group 'org-roam-daily-reflection
  :type 'sexp)

;;; main function
(defun org-roam-daily-reflect (&optional m n)
  "Show the previous `n' number of `m' time spans of org-roam dailies
from the current org-mode daily. choices for `n' are integers and choices
for `m' are \"day\", \"week\", \"fortnight\", \"month\", \"year\", \"decade\",
and \"century\"."
  (interactive)
  (unless (or (org-roam-dailies--daily-note-p) org-roam-daily-reflection-disable-org-roam-daily-check)
    (user-error "Not in a daily-note"))

  ;; ask user for `m' and `n' if called interactively
  (setq m (or m
           (when (called-interactively-p 'any)
             (intern (message "%s"
                              (completing-read "What time interval?"
                                               org-roam-daily-reflection-time-spans
                                               nil t nil nil))))
           'year))
  (setq n (or n
              (when (called-interactively-p 'any)
                (string-to-number (message "%s"
                                           (read-number
                                            (concat "How many "
                                                    (symbol-name m) "s? ")))))
              3))

  ;; Check if earliest date is compatible (TODO)
  ;; ....
    
  ;; First, create the needed splits (if possible)
  (org-roam-reflect--determine-splits n)

  ;; Then, start from the furthest back `m' (`n' `m's ago) date
  ;; and successively find+open the journal entry for those dailies:
  (cl-loop for i from (1- n) downto 1
           do (org-roam-reflect--open-prev-journal-entry
               (org-read-date nil nil (file-name-base buffer-file-name))
               i ;; start with greatest number for furthest back
               m))

  ;; finally, return the point to the top of the first buffer.
  (goto-char (point-min)))

(defun org-roam-reflect--determine-splits (no-of-splits)
  "Split the frame into `no-of-splits' number of windows in the
appropriate configuration."
  (let ((split-direction org-roam-daily-reflection-direction-of-window-splits))
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
                (cl-loop for i from (1- no-of-splits) downto 1 
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

(defun org-roam-reflect--prev-node-extant-file-p (org-date)
  "Determines whether an org-roam daily already exists for
`org-date' date."
  (file-exists-p
   (concat
    (expand-file-name org-roam-dailies-directory org-roam-directory)
    "/" org-date ".org")))

(defun org-roam-reflect--determine-prev-journal-entry (org-curr-date offset unit)
  "Find the appropriate org-roam daily journal that is `offset' number of 
`units' before `org-curr-date'."
  (let* ((ocd-date (decode-time (org-time-string-to-time org-curr-date)))
         (ocd-year (nth 5 ocd-date))
         (ocd-month (nth 4 ocd-date))
         (ocd-day (nth 3 ocd-date))
         (org-new-date nil))
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
      (let ((back-by
             (if (and (date-leap-year-p ocd-year)
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

(defun org-roam-reflect--open-prev-journal-entry (org-curr-date offset unit)
  "Open the appropriate org-roam daily journal that is `offset' number of 
`units' before `org-curr-date'."

  (let ((earlier-journal-entry
         (org-roam-reflect--determine-prev-journal-entry org-curr-date
                                                         offset
                                                         unit)))
  
    ;; Run `org-roam-dailies--capture' with non-nil GOTO optional arg
    ;; (this is the best way to open both existing and )
    ;; and go to the note without creating an entry;
    ;; this creates a daily-note for TIME (first arg) if necessary.
    (org-roam-dailies--capture
     (org-read-date nil t
                    earlier-journal-entry
                    nil) t)

    ;; Mark non-previously existing dailies as unmodified
    ;; (this prevents littering `buffers' with spurious would-be files).
    (unless (org-roam-reflect--prev-node-extant-file-p earlier-journal-entry)
      (set-buffer-modified-p nil))

    ;; move the point to the next window (right or down).
    (other-window 1)))

;;; various predefined reflection commands
(defun org-roam-reflect-on-last-three-years ()
  "Compare the daily for the current day to the same day on the
previous two years."
  (interactive)
  (org-roam-daily-reflect 'year 3))

(defun org-roam-reflect-on-last-three-months ()
  "Compare the daily for the last three months."
  (interactive)
  (org-roam-daily-reflect 'month 3))

(defun org-roam-reflect-on-last-two-fortnights ()
  "Compare the daily for today and the preceding fortnight."
  (interactive)
  (org-roam-daily-reflect 'fortnight 2))

(defun org-roam-reflect-on-last-four-weeks ()
  "Compare the daily for the last four weeks."
  (interactive)
  (org-roam-daily-reflect 'week 4))

(defun org-roam-reflect-on-last-five-decades ()
  "Compare the daily for the last half century."
  (interactive)
  (org-roam-daily-reflect 'decade 5))

(defun org-roam-reflect-on-last-five-days ()
  "Compare the daily for the last five days."
  (interactive)
  (org-roam-daily-reflect 'day 5))

(provide 'org-roam-daily-reflection)

;;; org-roam-daily-reflection.el ends here
