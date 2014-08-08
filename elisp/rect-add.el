;;; rect-add.el --- Rectangle Mode Additions
;;; Wilson Snyder, wsnyder@wsnyder.org
;;
;; COPYING:
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;;;======================================================================
;;;; Personal
;;;;======================================================================

(autoload 'apply-on-rectangle "rect")

(defun tab-rectangle (start end)
  "Insert a TAB on each line of the region-rectangle, shifting text right.
The left edge of the rectangle specifies the column for insertion.
This command does not delete or overwrite any existing text."
  (interactive "r")
  ;; I think this was fixed??
  ;;; If in the same start & end col, string-rectangle won't work right, so fix it
  ;;(if (eq (save-excursion (goto-char start) (current-column))
  ;;	  (save-excursion (goto-char end) (current-column)))
  ;;   (setq start (1+ start)))
  (string-rectangle start end "\t"))

;;;;======================================================================
;;;; Released to RMS
;;;;======================================================================

;;;###autoload
(defun yank-tilted-rectangle ()
  "Yank the last killed rectangle with upper left corner at point.
Yank is rotated 90 degrees so that a line becomes a column."
  (interactive)
  (let* ((new-rect nil)
	 (len (length (car killed-rectangle)))
	 (chr (1- len)))
    ;; Make one line containing the first character of the line
    (while (>= chr 0)
      (setq new-rect
	    (cons 
	     (mapconcat '(lambda (strg) 
			   (substring strg chr (1+ chr)))
			killed-rectangle "")
	     new-rect))
      (setq chr (1- chr)))
    ;;
    (insert-rectangle new-rect)
    ))

;;;###autoload
(defun numerate-rectangle (start end beginnum &optional increment)
  "Insert sequential numbers on each line of the region-rectangle,
shifting text right.
The left edge of the rectangle specifies the column for insertion.
Called from a program, takes three args; START, END, BEGINNUM and
optional INCREMENT."
  (interactive "r\nsStart number: ")
  (setq increment (or increment 1))
  (let ((num beginnum))
    (apply-on-rectangle
     '(lambda (startcol endcol &rest args)
	(move-to-column-force startcol)
	(let ((p (point)))
	  (move-to-column-force endcol)
	  (delete-region p (point)))
	(insert num)
	(setq num (int-to-string (+ increment (string-to-int num)))))
     start end)))

;;;###autoload
(defun sum-rectangle (start end)
  "Sum the column of numbers with corners at point and mark.
Calling from program, supply two args START and END, buffer positions.
With a prefix argument, inserts the resulting sum at point."
  (interactive "r")
  (let ((sum 0.0))
    (apply-on-rectangle
     '(lambda (startcol endcol &rest args)
	(setq sum (+ sum (string-to-number (buffer-substring
					    (progn (move-to-column startcol) (point))
					    (progn (move-to-column endcol) (point)))))))
     start end)
    (if current-prefix-arg
	(insert (number-to-string sum))
      (message (number-to-string sum)))
    sum))
