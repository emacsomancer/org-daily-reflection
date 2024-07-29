;;; org-roam-daily-reflection.el --- concurrent display of org-roam dailies  -*- lexical-binding: t; -*-

;; Org Roam Daily Reflection - compare N org-roam dailies at M intervals 

;; Copyright (C) 2024 Benjamin Slade

;; Author: Benjamin Slade <slade@lambda-y.net>
;; Maintainer: Benjamin Slade <slade@lambda-y.net>
;; URL: ?
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

;;; Installation:

;;; Usage:

;;; Advice:

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

(defcustom org-roam-daily-reflection-time-spans '("year" "month" "fortnight" "week" "day")
  "Possible time-spans. Defaults to \"year\", \"month\", \"fortnight\", \"week\", \"day\".
(Currently pointless/dangerous to change as the package won't know what to do with
other spans.)"
  :group 'org-roam-daily-reflection
  :type 'sexp)

;;; main function
(defun org-roam-daily-reflect (&optional m n)
  "Show the previous `n' number of `m' time spans of org-roam dailies
from the current org-mode daily. Choices for `m' are `year', `month',
`fortnight', `week', and `day'."
  (interactive)
  (unless (or (org-roam-dailies--daily-note-p) org-roam-daily-reflection-disable-org-roam-daily-check)
    (user-error "Not in a daily-note"))
  (setq m (or m
           (when (called-interactively-p 'any)
             (intern (message "%s"
                              (completing-read "What time interval?"
                                               org-roam-daily-reflection-time-spans nil t nil nil))))
           'year))
  (setq n (or n
              (when (called-interactively-p 'any)
                (string-to-number (message "%s"
                                           (read-number (concat "How many " (symbol-name m) "s? ")))))
              3))
  (delete-other-windows)
  (org-roam-reflect--determine-splits n) ;; create the splits
  (balance-windows)
  ;; start from the furthest back `m' (`n' `m's ago)
  ;; and successively find the journal entry for those dailies
  (cl-loop for i from (1- n) downto 1
           do (org-roam-reflect--open-prev-journal-entry
               (org-read-date nil nil (file-name-base buffer-file-name))
               i ;; start with greatest number for furthest back
               m))  
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
                (< (/ width height) 1)) ;; (/ (display-pixel-width) (display-pixel-height))
            ;; for above: actually, not floating point, so?
            (setq split-direction 'horizontal)
          (setq split-direction 'vertical))))
    (cl-loop for i from (1- no-of-splits) downto 1 
             do (if (equal split-direction 'vertical)
                    ;; (condition-case nil
                    (split-window-right)
                  ;; (progn (delete-other-windows)
                  ;; (error "can't do that")))
                  (split-window-below)))))

(defun org-roam-reflect--prev-node-extant-file-p (org-date)
  "Determines whether an org-roam daily already exists for
`org-date' date."
  (file-exists-p
   (concat
    (expand-file-name org-roam-dailies-directory org-roam-directory)
    "/" org-date ".org")))

(defun org-roam-reflect--open-prev-journal-entry (org-curr-date offset unit)
  "Find the appropriate daily `offset' number of `units' before
`org-curr-date'."
  (let* ((ocd-date (decode-time (org-time-string-to-time org-curr-date)))
         (ocd-year (nth 5 ocd-date))
         (ocd-month (nth 4 ocd-date))
         (ocd-day (nth 3 ocd-date))
         (org-new-date nil))
    (cond ((equal unit 'year)
           ;; if leap day, then go back 4 years to prev leap day
           (let ((back-by
                  (if (and (date-leap-year-p ocd-year)
                           (= ocd-month 02)
                           (= ocd-day 29))
                      (* offset 4)
                    offset)))
             (setq org-new-date (org-read-date nil nil
                                               (concat "--" (number-to-string back-by) "y")
                                               nil
                                               (org-time-string-to-time org-curr-date)))))
          ;; for the remaining units, go back 30 days, 2 weeks, 1 week, or 1 day, respectively.
          ((equal unit 'month)
           (let ((back-by (* offset 30)))
             (setq org-new-date (org-read-date nil nil
                                               (concat "--" (number-to-string back-by) "d")
                                               nil (org-time-string-to-time org-curr-date)))))
          ((equal unit 'fortnight)
           (let ((back-by (* offset 2)))
             (setq org-new-date (org-read-date nil nil
                                               (concat "--" (number-to-string back-by) "w")
                                               nil (org-time-string-to-time org-curr-date)))))
          ((equal unit 'week)
           (let ((back-by offset))
             (setq org-new-date (org-read-date nil nil
                                               (concat "--" (number-to-string back-by) "w")
                                               nil (org-time-string-to-time org-curr-date)))))
          ((equal unit 'day)
           (let ((back-by offset))
             (setq org-new-date (org-read-date nil nil
                                               (concat "--" (number-to-string back-by) "d")
                                               nil (org-time-string-to-time org-curr-date)))))
          (t (user-error "Unrecognised unit.")))
    ;; org-new-date
    ;; `org-roam-dailies--capture' with non-nil GOTO optional arg
    ;;   goes to the note without creating an entry;
    ;;   and creates a daily-note for TIME (first arg) if necessary.
    (org-roam-dailies--capture
     (org-read-date nil t
                    org-new-date
                    nil) t)
                    (unless (org-roam-reflect--prev-node-extant-file-p org-new-date)
                      (set-buffer-modified-p nil))
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

(defun org-roam-reflect-on-last-five-days ()
  "Compare the daily for the last five days."
  (interactive)
  (org-roam-daily-reflect 'day 5))

(provide 'org-roam-daily-reflection)

;;; org-roam-daily-reflection.el ends here
