; $Id: ebiff-signal.el,v 1.2 2003/02/04 19:11:40 wsnyder Exp wsnyder $
;; Patches to files.el
;; Files.el is compiled into emacs, so we can't just edit that

(provide 'files-patch)

(defun insert-directory (file switches &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This works by running a directory listing program
whose name is in the variable `insert-directory-program'.
If WILDCARD, it also runs the shell specified by `shell-file-name'."
  ;; We need the directory in order to find the right handler.
  (let ((handler (find-file-name-handler (expand-file-name file)
					 'insert-directory)))
   (if handler
	(funcall handler 'insert-directory file switches
		 wildcard full-directory-p)
      (if (eq system-type 'vax-vms)
	  (vms-read-directory file switches (current-buffer))
	(let* ((coding-system-for-read
		(and enable-multibyte-characters
		     (or file-name-coding-system
			 default-file-name-coding-system)))
	       ;; This is to control encoding the arguments in call-process.
	       (coding-system-for-write coding-system-for-read)
	       (result
		(if wildcard
		    ;; Run ls in the directory of the file pattern we asked for
		    (let ((default-directory
			    (if (file-name-absolute-p file)
				(file-name-directory file)
			      (file-name-directory (expand-file-name file))))
			  (pattern (file-name-nondirectory file)))
		      (call-process
		       shell-file-name nil t nil
		       "-c" (concat (if (memq system-type '(ms-dos windows-nt))
					""
				      "\\") ; Disregard Unix shell aliases!
				    insert-directory-program
				    " -d "
				    (if (stringp switches)
					switches
				      (mapconcat 'identity switches " "))
				    " -- "
				    ;; Quote some characters that have
				    ;; special meanings in shells; but
				    ;; don't quote the wildcards--we
				    ;; want them to be special.  We
				    ;; also currently don't quote the
				    ;; quoting characters in case
				    ;; people want to use them
				    ;; explicitly to quote wildcard
				    ;; characters.
				    (shell-quote-wildcard-pattern pattern))))
		  ;; SunOS 4.1.3, SVr4 and others need the "." to list the
		  ;; directory if FILE is a symbolic link.
		  (apply 'call-process
			 insert-directory-program nil t nil
			 (append
			  (if (listp switches) switches
			    (unless (equal switches "")
			      ;; Split the switches at any spaces so we can
			      ;; pass separate options as separate args.
			      (split-string switches)))
			  ;; Avoid lossage if FILE starts with `-'.
			  '("--")
			  (progn
			    (if (string-match "\\`~" file)
				(setq file (expand-file-name file)))
			    (list
			     (if full-directory-p
				 (concat (file-name-as-directory file) ".")
			       file))))))))
	  (if (/= result 0)
	      ;; We get here if `insert-directory-program' failed.
	      ;; On non-Posix systems, we cannot open a directory, so
	      ;; don't even try, because that will always result in
	      ;; the ubiquitous "Access denied".  Instead, show them
	      ;; the `ls' command line and let them guess what went
	      ;; wrong.
	      (if (and (file-directory-p file)
		       (memq system-type '(ms-dos windows-nt)))
		  (error
		   "Reading directory: \"%s %s -- %s\" exited with status %s"
		   insert-directory-program
		   (if (listp switches) (concat switches) switches)
		   file result)
		;; Unix.  Access the file to get a suitable error.
		(access-file file "Reading directory"))
	    ;; Replace "total" with "used", to avoid confusion.
	    ;; Add in the amount of free space.
	    (save-excursion
	      (goto-char (point-min))
	      (when (re-search-forward "^total" nil t)
		(replace-match "used")
		(end-of-line)
		;; WPS PATCH, too slow on cygwin
		(if nil
		 (let (available)
		   (with-temp-buffer
		    (call-process "df" nil t nil ".")
		    (goto-char (point-min))
		    (forward-line 1)
		    (skip-chars-forward "^ \t")
		    (forward-word 3)
		    (let ((end (point)))
		      (forward-word -1)
		      (setq available (buffer-substring (point) end))))
		  (insert " available " available)))))))))))
