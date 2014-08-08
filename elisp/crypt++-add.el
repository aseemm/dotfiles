;; crypt++-add -- Crypt mode support for windows NT and clearing password
;; $Id$

(provide 'crypt++-add)

(require 'crypt++)

;;;
;;; Variables
;;;

(defvar crypt-encryption-filename-regexp
  "\\(\\.pgp$\\|\\.wgp$\\|\\.bku$\\)")
  "Regexp that matches files that should be encrypted."

(defvar crypt-timer-interval
  (* 1 10 60 )
  "Seconds of idle time before flushing password cache.")

(defvar crypt-rename-alist
  nil
  "List of (oldname newname) pairs for renaming.")

(defvar crypt-rename-directories
  nil
  "List of directories for renaming.")

;;;
;;; Function changes
;;;

(defun crypt-build-encryption-alist ()
  ;; Returns the encryption alist
  (list
   ;; crypt
   (list 'crypt
         crypt-encryption-magic-regexp crypt-encryption-magic-regexp-inverse
         (or crypt-encryption-file-extension "\\(\\.wgp\\|\\.WGP\\|\\.bku\\|\\.BKU\\)$")
	 (cond ((eq system-type `windows-nt)
		"h:\\home\\bin\\wgp.exe")
	       (t "~/bin/wgp"))
	 (cond ((eq system-type `windows-nt)
		"h:\\home\\bin\\wgp.exe")
	       (t "~/bin/wgp"))
         '("-c" "-" "-z")
         '("-" "-z")
         "PGP"
         nil
         t
         )
   ;; Add new elements here ...
   ))

(defun crypt-encoded-p (&optional buffer)
  nil)
(defun crypt-encrypted-p (&optional buffer)
  ;; Returns t if current buffer, or optionally BUFFER, is encrypted.
  ;; Look for MAGIC-REGEXP and absence of MAGIC-REGEXP-INVERSE.  If so, then
  ;; assume it is an encrypted buffer.
  ;; Sets `crypt-buffer-encryption-key' to password if not set already.

  ;; Do not try to decrypt buffer if not wanted.
  (if (not crypt-never-ever-decrypt)

      (save-excursion
        (and buffer (set-buffer buffer) buffer-file-name)

        (save-restriction
          (widen)

	  ;; Check all encryption conditions.  If any fail, then return nil
	  ;; value of this if-form, else check for password.
	  (if (string-match crypt-encryption-filename-regexp buffer-file-name)
	      (progn
		;; Get key, i.e., the password?
		(or crypt-buffer-encryption-key
		    ;; Do not confirm on reading an encrypted file.
		    (let ((crypt-confirm-password nil))
		      (call-interactively 'crypt-set-encryption-key)
		      (setq-default crypt-buffer-encryption-key
				    crypt-buffer-encryption-key)
		      ))
		t))))))

(defun crypt-erase-encryption-key ()
  "Erase all traces of the key"
  (interactive)
  (setq-default crypt-buffer-encryption-key nil
		crypt-hold nil)
  (mapcar (function (lambda (elt)
		      (set-buffer elt)
		      (setq crypt-buffer-encryption-key nil
			    crypt-hold nil )))
	  (buffer-list))
  (garbage-collect)
  )

(defun crypt-kill-encrypted-buffers ()
  "Erase all buffers that were encrypted and key.
Intended to be called every few hours or so for security."
  (interactive)
  (mapcar (function (lambda (elt)
		      (set-buffer elt)
		      (cond ((and buffer-file-name
				  (crypt-encrypted-p)
				  (not (buffer-modified-p)))
			     ;;
			     (kill-buffer elt)))))
	  (buffer-list))
  (garbage-collect)
  )

(defvar crypt-timer-running nil "Is the crypt timer running?")
(defun crypt-timer ()
  "Periodic clearing timer for erasing keys."
  (interactive)
  (message "Clearing crypt buffers")
  (crypt-kill-encrypted-buffers)
  (crypt-erase-encryption-key)
  (if (get-buffer "*Messages*")
      (kill-buffer "*Messages*")) ; Eliminate hint as to PW length
  (cond ((not crypt-timer-running)
	 (run-with-idle-timer crypt-timer-interval t 'crypt-timer)
	 (setq crypt-timer-running t))))


;;;
;;; NT support
;;;

(defun cryptadd-process-region (start end prog args)
  (if (member "-c" args)
      (cryptadd-pgp-crypt-region-PGP start end prog nil)
    (cryptadd-pgp-crypt-region-PGP start end prog t))
  (setq buffer-file-coding-system `undecided-unix)
  )

(defun cryptadd-pgp-crypt-region-WGP (start end prog decrypt)
  (let* ((tmp-buf (get-buffer-create "*wgpinfo*"))
	 (binary-process-input t) ; Win32
	 (binary-process-output t) ; Win32
	 )
    ;;
    (when (not decrypt)
      (setq crypt-hold (buffer-substring start end))
      )
    ;;
    (save-excursion (set-buffer tmp-buf) (erase-buffer)
		    (setq buffer-file-coding-system `undecided-unix))
    (unless decrypt
      (call-process-region start (point-max)
			   (expand-file-name prog)
			   t (current-buffer) nil
		      "-c"
		      (concat "-z" crypt-buffer-encryption-key)
		      "-"))
    ;; Decrypt
    (when decrypt
      (message "%s %s %s"
	       (expand-file-name prog)
	       (concat "-z" crypt-buffer-encryption-key)
	       "-")
      (call-process-region start (point-max)
			   (expand-file-name prog)
			   t (current-buffer) nil
			   (concat "-z" crypt-buffer-encryption-key)
			   "-"))
    ))

(defun cryptadd-pgp-crypt-region-PGP (start end prog decrypt)
  (let* ((tmp-file     (concat (or (getenv "TEMP") "/tmp") "/pgpstuff"))
	 (tmp-pgp-file (concat (or (getenv "TEMP") "/tmp") "/pgpstuff.wgp"))
	 (tmp-buf (get-buffer-create " *pgpinfo*"))
	 (in-file  (if decrypt tmp-pgp-file tmp-file))
	 (out-file (if decrypt tmp-file tmp-pgp-file))
	 )
    (if (file-exists-p tmp-file) (delete-file tmp-file))
    (if (file-exists-p tmp-pgp-file) (delete-file tmp-pgp-file))
    (if (file-exists-p in-file)	(delete-file in-file))
    (if (file-exists-p out-file) (delete-file out-file))
    ;;
    (write-region start end in-file)
    ;;
    (when (not decrypt)
      (setq crypt-hold (buffer-substring start end))
      (sleep-for 1)
      )
    ;;
    (save-excursion (set-buffer tmp-buf) (erase-buffer) (setq buffer-file-coding-system `undecided-unix)
)
    (unless decrypt
      (call-process   (expand-file-name prog) nil tmp-buf nil
		      "+batchmode" "+verbose=0"
		      "-c"
		      (concat "-z" crypt-buffer-encryption-key)
		      tmp-file))
    ;; Decrypt
    (call-process (expand-file-name prog) nil tmp-buf nil
		  "+batchmode" "+verbose=0"
		  (concat "-z" crypt-buffer-encryption-key)
		  tmp-pgp-file)
    ;;
    (unless decrypt
      (save-excursion
	(with-temp-buffer
	  (erase-buffer)
	  (insert-file tmp-file)
	  (unless (equal (buffer-string) crypt-hold)
	    (error "Saving error")))))
    ;;
    (delete-region start end)
    (cond ((file-exists-p out-file)
	   (insert-file out-file))
	  (t
	   (insert-buffer tmp-buf)
	   ))
    ;; Overwrite before delete for saftey
    (write-region (point-min) (point-min) in-file)
    (write-region (point-min) (point-min) out-file)
    (if (file-exists-p tmp-file) (delete-file tmp-file))
    (if (file-exists-p tmp-pgp-file) (delete-file tmp-pgp-file))
    (if (file-exists-p in-file)	(delete-file in-file))
    (if (file-exists-p out-file) (delete-file out-file))
    ))

(defun crypt-encrypt-region (start end key &optional decrypt)
  "Encrypt region START to END using KEY and `crypt-encryption-type'.  When
called interactively START and END default to point and mark \(START being the
lesser of the two\), and KEY is prompted for.  With optional DECRYPT non-nil,
decryption is done."

  (interactive
   (let (decrypt)
     (barf-if-buffer-read-only)
     (list (region-beginning)
           (region-end)
           (crypt-read-string-no-echo
            (concat (if (setq decrypt (y-or-n-p "Decrypt region? ")) "De" "En")
                    "crypt buffer using key: ")
            ;; Do not confirm on decrypting region.
            (if (not decrypt) crypt-confirm-password))
           decrypt)))

  ;; WPS: Make backups; don't trust this process
  (make-local-variable `make-backup-files)
  (make-local-variable `backup-by-copying)
  (setq make-backup-files t
	backup-by-copying nil)

  (crypt-save-point

   ;; We define the PROGRAM as the encryption program or decryption program
   ;; listed for `crypt-encryption-type' of `crypt-encryption-alist.'  These
   ;; should be just the name of the executable and should _not_ contain any
   ;; arguments.  `(call-process-region)' would be confused if we tried to
   ;; pass the arguments as part of the PROGRAM.  The arguments are passed
   ;; through the encryption args or decryption args listed for
   ;; `crypt-encryption-type' of `crypt-encryption-alist.'

   ;; Thanks to Joe Ilacqua <spike@world.std.com> and others for pointing out
   ;; an error that occurs with some encryption programs (e.g., the crypt from
   ;; Sun Microsystems, HPUX-8, and BSD) if `args' is `"".'  This will allow
   ;; nil values and lists of strings for argument.

   (let (prog args)

     ;; Get the proper program and arguments.
     (if decrypt
         (setq prog (crypt-get-decoding-program crypt-encryption-type)
               args (crypt-get-decoding-args crypt-encryption-type))
       (setq prog (crypt-get-encoding-program crypt-encryption-type)
             args (crypt-get-encoding-args crypt-encryption-type)))

     ;; Check arguments.
     (cond
      (t;nil;(not (eq system-type `windows-nt))
       (cryptadd-process-region start end prog args))

      ;; nil or "" args - don't pass.
      ((or (not args) (equal "" args))
       (call-process-region start end prog t `(t nil) nil key))

      ;; Check if the args are in the form of a list - must use apply.
      ((listp args)
       (apply 'call-process-region
              (append (list start end prog t `(t nil) nil) args (list key))))

      ;; Default - just a non-null string.
      (t
       (call-process-region start end prog t t nil args key))))))

;;;
;;; Mail support
;;;

(defun crypt-pgp-decrypt ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (cond ((search-forward "-BEGIN PGP MESSAGE-" nil t)
	   (let ((stpt (progn (forward-line 1) (point)))
		 (endpt))
	     (goto-char stpt)
	     (insert "-----BEGIN PGP MESSAGE-----\n")
	     (search-forward "-END PGP MESSAGE-")
	     (forward-line 1)
	     (setq endpt (point))
	     (insert "-----END PGP MESSAGE-----\n")
	     ;;
	     (or crypt-buffer-encryption-key
		 (call-interactively 'crypt-set-encryption-key))
	     (shell-command-on-region stpt endpt
				      (concat "pgp -f -z " crypt-buffer-encryption-key)
				      nil t))))))

(defun crypt-rmail-decrypt ()
  "Show decrypted message."
  (interactive)
  (narrow-to-region (rmail-msgbeg rmail-current-message) (point-max))
  (let ((buffer-read-only nil))
    (crypt-pgp-decrypt)))

;;;
;;; Directory Crypting
;;;

(defun crypt-directory-under-control (dir)
  "Return true if directory is under crypt control"
  (member dir crypt-rename-directories))

(defun crypt-read-hook-file (hook-filename)
  (cond ((file-exists-p hook-filename)
	 (save-window-excursion
	   (find-file hook-filename)
	   (eval-buffer)))))

;;DISABLED (add-hook 'dired-after-readin-hook 'crypt-directory-renames)
(defun crypt-directory-renames ()
  ;; HOOK DISABLED ABOVE
  (let ((check-alist dired-subdir-alist))
    (while check-alist
      ;; check for hook file... read automatically
      (let ((hook-filename (expand-file-name "0hook.el.wgp" (car (car check-alist)))))
	(crypt-read-hook-file hook-filename))
      (let ((hook-filename (expand-file-name "../0hook.el.wgp" (car (car check-alist)))))
	(crypt-read-hook-file hook-filename))
      ;; rename any files
      (cond ((crypt-directory-under-control (car (car check-alist)))
	     (crypt-do-renames)
	     (if (featurep 'hilit19)
		 (hilit-repaint-command nil))
	     ))
      ;; next
      (setq check-alist (cdr check-alist))
      )))

;;DISABLED  (add-hook 'find-file-hooks 'cryptdir-find-file-hook)
(defun cryptdir-find-file-hook ()
  (cond ((crypt-directory-under-control (file-name-directory (buffer-file-name)))
	 (let ((newname (nth 1 (assoc (file-name-nondirectory (buffer-file-name)) crypt-rename-alist))))
	   (if newname
	       (rename-buffer (generate-new-buffer-name newname t))
	     )))))

(defun crypt-do-renames ()
  (save-excursion
    (let ((rn crypt-rename-alist)
	  (buffer-read-only nil))
      (while rn
	(goto-char (point-min))
	(cond ((search-forward (nth 0 (car rn)) nil t)
	       (beginning-of-line)
	       (search-forward "-")
	       (replace-match "l")
	       (end-of-line)
	       (insert " -> ")
	       (insert (nth 1 (car rn)))
	       ))
	(setq rn (cdr rn))))))

;;;
;;; Startup
;;;

;; Timer
(crypt-rebuild-tables)
(crypt-timer)
(setq-default crypt-encoded-disable-auto-save t
	      crypt-auto-write-buffer t)



