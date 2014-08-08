;;; dired-add.el --- Extensions to directory editing mode
;;; Wilson Snyder, snyuder@ricks.enet.dec.com
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

;(defun dired-find-file ()
;  "In dired, visit the file or directory named on this line.  If the file represents the most recent VMS version of the file, the version number is ignored."
;  (interactive)
;;; Patched 6/29/92 by Wilson Snyder to add sans-version BREAKS if not newest version
;  (if (or (not (equal system-type 'vax-vms))
;	   (file-name-most-recent-version-p (dired-get-filename)))
;      (find-file (file-name-sans-versions (dired-get-filename)))
;    (progn
;      (message "Warning: Editing older version of file.")
;      (find-file (dired-get-filename)))))

(defun dired-random-file ()
  (interactive)
  (goto-char (point-min))
  (search-forward " .." nil t)
  (forward-line 1)
  (forward-line (random (count-lines (point) (point-max))))
  (dired-move-to-filename))

;;; 
;;; Lisp creation
;;;

;; In new-lisp/dired-x.el
(defvar dired-do-lisp-column 56)
(cond
 ((not (fboundp 'dired-do-lisp))

  (define-key dired-mode-map "I" 'dired-do-lisp)

  ;;;;###autoload
  (defun dired-do-lisp (&optional arg)
    "Create lisp code for the marked (or next ARG) files."
    (interactive "P")
    (let ((directory dired-directory)
	  buf)
      (save-excursion
	(setq buf (get-buffer-create "dired-lisp"))
	(set-buffer buf)
	(emacs-lisp-mode)
	(erase-buffer)
	(make-variable-buffer-local 'default-directory)
	(setq default-directory directory)
	;;
	(insert "(progn\n"
		"  (defun perfile (basea baseb dir)\n"
		"    (let ((a (concat dir basea))\n"
		"          (b (concat dir baseb)))\n"
		"      (cond ((not (equal a b))\n"
		"             (message b)\n"
		"             (rename-file b a)))))\n"
		))
      ;;
      (dired-map-over-marks
       (let* ((file (dired-get-filename))
	      (base (file-name-nondirectory file))
	      (dir  (file-name-directory file)))
	 (save-excursion
	   (set-buffer buf)
	   (insert "  (perfile \"" base "\"\t")
	   (indent-to dired-do-lisp-column)
	   (insert "\"" base "\"\t")
	   (indent-to (+ dired-do-lisp-column 32))
	   (insert "\"" dir "\")\n")))
       arg nil)
      ;;
      (switch-to-buffer buf)
      (insert "  )\n")))
  ))
  
;; In emacs 20.3
;;  (define-key dired-mode-map "%g" 'dired-mark-files-containing-regexp)
;;  (defun dired-mark-files-containing-regexp (regexp &optional marker-char)

;; 2009/03/18 WPS
(defun dired-do-elf-disassemble ()
  "Disassemble object/elf current dired file."
  (interactive)
  (elf-disassemble (dired-get-filename)))
  
(defun elf-disassemble (file)
  "Run disassembly on specified object/elf FILE, put output in temp buffer."
  (interactive "fFile to disassemble: ")
  ;; Someday we'll use the 'file' command to determine if it's x86/MIPS
  (let ((cmd (concat "mips_objdump -d " file "&"))
	(buf (concat "*disassemble " (file-name-nondirectory file) "*")))
    (save-excursion
      (with-output-to-temp-buffer buf
	(set-buffer buf)
	(setq default-directory (file-name-directory file))
	(call-process-shell-command cmd nil t t)))))

(define-key dired-mode-map "E" 'dired-do-elf-disassemble)
