; -*- Emacs-Lisp -*-
;;

;; Types
(setq at-winnt-p (or (eq system-type `windows-nt)
		     (eq system-type `cygwin)))
(setq at-company-p (string-match "caveonetworks.com" (system-name)))
(setq at-work-p  (or at-company-p))
(setq mail-ok-p  (file-readable-p (expand-file-name "~/Mail/.wmail_config.pl")))

;; Load-path before group startup
(setq my-load-path (list (expand-file-name "~/elisp")
			 ))
(setq load-path (append my-load-path load-path))

;; Work Group Startup
(cond ((boundp `site-start-version))
      ((file-exists-p "~/elisp/site-start.el")
       (load-file "~/elisp/site-start.el")))


;; (when (or at-veripool-p at-svaha-p at-winnt-p)
;;   (load "site-start" nil t))
;; ;; yes, again, so it's first in search paths
;; (setq load-path (append my-load-path load-path))

;; (or (boundp `site-start-version) (error "Wrong/no site-start file executed!"))

;;
;; Windows
(when window-system
  (let ((mach-title (concat
		     (if (= (user-real-uid) 0) "Root " "E ")
		     (progn (string-match "\\([^.]+\\)" (system-name))
			    (match-string 1 (system-name))))))
    (setq frame-title-format mach-title)) ;(multiple-frames "%b" ("" invocation-name "@" system-name))
  (setq default-frame-alist
  	(append (list
  		 ;; 87% 78% 73%
  		 ;; '(background-color . "#e1e1c8c8bbbb")
  		 ;; '(scroll-bar-background . "#e1e1c8c8bbbb")
  		 ;; '(foreground-color . "#000000000000")
  		 ;;(cons (cons 'font "-misc-fixed-medium-r-normal--13-*-*-*-c-70-iso8859-1")
  		 '(vertical-scroll-bars . right)
  		 '(tool-bar-lines . 0)
  		 ;;'(icon-type . t)
  		 )
  		default-frame-alist)))
;; Always show frame name as the machine name
(setq-default mode-line-frame-identification
	      ;; '("-%F  "))
	      (concat "-"
		      (progn (string-match "\\([^.]+\\)" (system-name))
			     (match-string 1 (system-name)))
		      "  "))


;; Lisp mode stuff
(add-hook 'emacs-lisp-mode-hook 'visit-emacs-lisp-tags-table)

;; Html mode
;(setq html-quotify-hrefs-on-find nil)
;(add-hook 'html-mode-hook
;	  '(lambda ()
;	     (make-variable-buffer-local `compile-command)
;	     (setq compile-command (concat "htmlpp " (file-name-nondirectory buffer-file-name)))))

;; Asm mode
(add-hook 'asm-mode-hook
	  '(lambda ()
	     (local-set-key ";" 'self-insert-command)))

;; C-Mode stuff
;;(load "c-mode")
(setq-default c-tab-always-indent nil)
;; Styles must be set before group-c-mode, so appears later in hook list
;;(when at-company-p
;;  (add-hook 'c-mode-common-hook '(lambda ()
;;				   (c-set-style "gnu"))))
;;(group-c-mode)
(add-hook 'c-mode-common-hook '(lambda ()
;;				   ;;(require 'cmode-add)
;;				   (set-c-style "Stroustrup")
;;				   (abbrev-mode 0)
;;				   (local-set-key "\C-c\C-c" 'electric-command-history)
				   (local-set-key "\C-c\C-s" 'save-and-compile)
				   (when (and (buffer-file-name)
					      (string-match "\\.y$" (buffer-file-name)))
				     (c-no-electric))))

(add-hook 'python-mode-hook '(lambda ()
			       (local-set-key "\C-c\C-s" 'save-and-compile)))

;;
;; Shell stuff
(defun comint-yank-end (&optional arg)
  (interactive)
  (goto-char (point-max))
  (yank arg)
  )
(defun my-comint-previous-input (arg)
  "Cycle backwards through input history if at end of buffer, 
otherwise simply call previous-line."
  (interactive "*p")
  (if (not (comint-after-pmark-p))
      (previous-line arg)
    (comint-previous-matching-input "." arg)))

(defun my-comint-next-input (arg)
  "Cycle forwards through input history if at end of buffer, 
otherwise simply call next-line."
  (interactive "*p")
  (if (not (comint-after-pmark-p))
      (next-line arg)
    (comint-previous-input (- arg))))
(setq-default comint-scroll-to-bottom-on-input t)
(add-hook 'shell-mode-hook
	  '(lambda ()
	     (local-set-key "\C-n" 'comint-next-input)
	     (local-set-key "\C-p" 'comint-previous-input)
	     (local-set-key "\C-y" 'comint-yank-end)
	     (local-set-key [up] 'my-comint-previous-input)
	     (local-set-key [down] 'my-comint-next-input)
	     (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
;;22.1 breaks     (setq comint-output-filter-functions
;;		   (list 'comint-carriage-motion
;;			 'comint-watch-for-password-prompt))
	     (setq font-lock-keywords
		   `(t
		     ("^[ /\n]*aseemm[^ \n]*"
		      (0 font-lock-warning-face))))))

;; ;;
;; ;; Mail stuff
;; (unless at-winnt-p
;;   (require 'rmail-add)
;;   (require 'etach))
;; (defun rmail-nope ()
;;   (interactive)
;;   (display-time-update)
;;   (error "RMail not supported on this system"))
;; (when at-work-p
;;     (setq sendmailadd-force-work 1))
;; (if (not (memq system-type '(amigados vax-vms)))
;;     (progn
;;       (setq mail-archive-file-name "~aseemm/Mail/.mboxout"
;; 	    mail-yank-ignored-headers "^Content-Type:\\|^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^message-id:\\|^summary-line:\\|^to:\\|^subject:\\|^in-reply-to:\\|^return-path:\\|^Path:\\|^Posting-Version:\\|^Article-I.D.:\\|^Expires:\\|^Date-Received:\\|^References:\\|^Control:\\|^Xref:\\|^Lines:\\|^Posted:\\|^Relay-Version:\\|^Message-ID:\\|^Nf-ID:\\|^Nf-From:\\|^Approved:\\|^Sender:\\|^Date:\\|^Followup-To:\\|^Distribution:\\|^Nntp-Posting-Host:\\|^X-\\|^Newsgroups:\\|^From:\\|^MIME-Version\\|^DKIM-Signature\\|^DomainKey-Signature")
;;       (setq rmail-file-name (if (eq system-type `windows-nt)
;; 				(expand-file-name "~aseemm/Mail/RMAIL_NT")
;; 			      (expand-file-name "~aseemm/Mail/RMAIL"))
;; 	    rmail-dont-reply-to-names "^aseemm\\|^aseem\.maheshwari"
;; 	    rmail-delete-after-output t
;; 	    rmail-secondary-file-directory "~aseemm/Mail/"
;; ;	    rmail-ignored-headers ".*"
;; ;	    rmail-displayed-headers "^Date:\\|^From:\\|^To:\\|^Cc:\\|^Resent-From:\\|^Resent-To:\\|^Subject:\\|^Newsgroups:\\|^Organization:"
;; 	    rmail-ignored-headers (concat rmail-ignored-headers "\\|^X-\\|^Message-\\|^Thread-\\|^Mail\\|^content-\\|^DKIM-Signature\\|^DomainKey-Signature\\|^MIME-")
;; 	    rmail-nonignored-headers "^$"  ;; x-spam-status on Emacs 22
;; 	    mail-yank-prefix ">"
;; 	    mail-signature "\n-Aseem\n"
;; 	    mail-from-style 'parens
;; 	    mail-self-blind t
;; 	    mail-archive-file-name nil
;; 	    user-full-name "Aseem Maheshwari"
;; 	    user-mail-address-ext      "aseemm@gmail.com"
;; 	    user-mail-address-work (cond (at-company-p "aseemm@company.com")
;; 					 (t "aseemm@unknown-work.unknown"))
;; 	    )
;;       (setq user-mail-address (if at-work-p user-mail-address-work
;; 				user-mail-address-ext))
;;       (setq etach-prompt-me-for-file-names t
;; 	    etach-clean-decoded-plain-text t
;; 	    etach-fill-decoded-plain-text nil
;; 	    etach-detachment-default-directory "~aseemm/backpack/frhere")

;;       (add-hook 'rmail-mode-hook '(lambda ()
;; 				    (require 'rmail-add)
;; 				    (require 'ebiff-signal)
;; 				    (setq truncate-lines nil)
;; 				    ))

;;       (add-hook 'rmail-edit-mode-hook '(lambda ()
;; 				    (setq fill-column 60)
;; 				    ))

;;       ;;rmail-output-file-alist
;;       ;;(setq rmail-primary-inbox-list "~aseemm/mbox" "/usr/spool/mail/$USER" "/usr/mail/$USER")
;;       ))

;; ;; Mail
;; (add-hook 'mail-mode-hook '(lambda ()
;; 			     (when spell-ok-p (flyspell-mode))
;; 			     (setq fill-column 60)
;; 			     (require 'sendmail-add)))

;; ;; Outline
;; (add-hook 'outline-mode-hook '(lambda ()
;; 				(when spell-ok-p (flyspell-mode))))

;; ;; Latex
;; (add-hook 'latex-mode-hook '(lambda ()
;; 			      (when spell-ok-p (flyspell-mode))))

;; ;; Text
;; (add-hook 'text-mode-hook '(lambda ()
;; 			     (local-set-key "\t" 'self-insert-command)))

;; Verilog-Mode
;(setq verilog-linter "vlint --brief --pragmas --andsynth ")
;(setq verilog-linter "vlint --brief --pragmas ")
(setq verilog-linter "cn_lint ")
(add-hook 'verilog-mode-hook
	  '(lambda ()
	     ;; Screws up verilog regexps
	     (setq compilation-error-regexp-alist
		   (delete 'gnu compilation-error-regexp-alist))))

;;;
;;; DIRED

(add-hook 'dired-mode-hook '(lambda ()
			      (define-key dired-mode-map "\M-?" 'dired-random-file)))

;; ;; Diary
;; (when (not (memq system-type '(amigados vax-vms)))
;;   (setq diary-file "~aseemm/docs/Common/diary.pgp")
;;   (add-hook 'diary-display-hook 'appt-make-list)
;;   ;;(setq hebrew-holidays nil)
;;   ;;(setq all-hebrew-calendar-holidays t)
;;   (setq islamic-holidays nil)
;;   (require 'holidays)
;;   (require 'diary-lib)
;;   (display-time)
;;   (setq display-time-hook nil)	; Turn off appointment checking
;;   (autoload 'print-calpage "calendar-add" "Calendar printing." t)
;;   (setq calendar-longitude -71.48488
;; 	calendar-latitude 42.38587)
;;   )

;; Change filling column
(setq-default fill-column 75)

;; No startup message
(setq inhibit-startup-message t)

;; Enable Commands
(put 'eval-expression 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)
(setq apropos-do-all t)   ;; Fast machine!

(setq auto-save-list-file-prefix nil)  ;; No .saves_...
(setq auto-save-interval 1000)
(setq auto-save-size-limit 1000000)
(setq garbage-collection-messages t)

;; Aliases
(defalias 'mkdir 'make-directory)

;; Crypt hooks
;; (when (file-exists-p (expand-file-name "~/elisp/crypt++-add.el"))
;;   ;; Uncompress .gz & .Z's
;;   (require 'jka-compr)
;;   ;; Crypt .pgp files
;;   (require 'crypt++-add)
;;   ;;
;;   (setq crypt-timer-interval (* 10 60))
;;   )

; Buffer Display
(defun list-buffers (&optional files-only)
  "List buffers, switching to the list.  (See list-buffers-noselect for more info."
  (interactive "P")
  (switch-to-buffer (list-buffers-noselect files-only)))
;; something wrong with buffer lists in built-in functions
;; TODO: support buffer creation order! (as in Tab-list in some www browsers)
(defun my-buffer-next ()
  "Primitive buffer navigation function: next-buffer."
  (interactive)
  (bury-buffer) ;; (switch-to-buffer (other-buffer))
  (my-display-prev-next-buffers))

(defun my-buffer-prev ()
  "Primitive buffer navigation function: prev-buffer."
  (interactive)
  (switch-to-buffer (car (last (buffer-list))))
  (my-display-prev-next-buffers))

(defun my-display-prev-next-buffers ()
  "Show two previous, current and two next buffer names in the minibuffer.
Example:
-2:*Messages* -1:*Help*    0:.emacs      1:*info*  2:*scratch*"
  (interactive)
  (let ((n -3))
    (message "%s"
             (mapconcat
              (lambda (x)
                (setq n (+ n 1))
                (format "%d:%-12s"
                        n (substring (buffer-name x) 0 (min (length (buffer-name x)) 11))))
              (append
               (last (buffer-list) 2)
               (reverse (last (reverse (buffer-list)) 3)))
              " "))))

;; Emacs 20.4 bugs:
;; View mode is hosed with electic-command-history
;; (unless emacs-type-xemacs
;;   (defun view-mode ())
;;   (setq view-mode-map nil))

;; Info
;;(setq Info-additional-directory-list
;;      (list (expand-file-name "~aseemm/src/dinotrace/vnew")
;;	    (expand-file-name "/usr/info")))

;; Books
(autoload 'books-mode "books" "Mode for editing booklist." t)

;; Grep
;;(setq grep-command "wgrep -W -S  -I -nH -r ")   ;; -e seems to fail if say "-e '(a|b|c)'"
(setq grep-command "wgrep -W -S  -I -nH -r -e ")  ;; but grep-defaulted now expects -e
(add-hook 'grep-mode-hook (lambda ()
			    (setq buffer-read-only nil)
			    (buffer-enable-undo)))
(add-hook 'occur-mode-hook (lambda ()
			     (setq buffer-read-only nil)
			     (buffer-enable-undo)))
;; Patched this:
;;/usr/share/emacs/22.1/lisp/replace.el
;;            (setq buffer-read-only nil)  ;; Changed t to nil -aseemm

;; Cursor
(setq blink-cursor-delay 30)
(blink-cursor-mode 0)

;; Font-lock
;; (when (and window-system (>= emacs-major-version 20))
;;   (cond (emacs-type-xemacs
;; 	 (setq font-lock-mode-disable-list '(books-mode art-mode sim-log-mode))
;; 	 )
;; 	(t
;; 	 (setq font-lock-global-modes '(not books-mode art-mode sim-log-mode))
;; 					;rmail-mode rmail-summary-mode
;; 	 (global-font-lock-mode 1 nil)
;; 	 ))
;;   ;; Verilog-mode.el is just over the default 256K limit
;;   (setq font-lock-maximum-size 300000)
;;   ;; Don't use light colors, I can't see them
;;   (custom-set-faces
;;    '(font-lock-string-face ((t (:foreground "Brown"))))
;;    '(font-lock-builtin-face ((((class color) (background light)) (:foreground "VioletRed"))))
;;    '(font-lock-comment-face ((((class color) (background light)) (:foreground "Firebrick"))))
;;    '(font-lock-reference-face ((t (:foreground "MediumBlue"))))
;;    '(font-lock-string-face ((t (:foreground "Brown"))))
;;    '(font-lock-keyword-face ((((class color) (background light)) (:foreground "Purple"))))
;;    '(font-lock-warning-face ((t (:bold t :foreground "OrangeRed"))))
;;    '(font-lock-type-face ((t (:foreground "DarkGreen"))))
;;    '(font-lock-variable-name-face ((t (:foreground "Maroon"))))
;;    '(font-lock-function-name-face ((((class color) (background light)) (:foreground "Blue"))))
;;    '(font-lock-builtin-face ((((class color) (background light)) (:foreground "VioletRed"))))
;;    ))

(defun del ()
  (interactive)
  (define-key global-map "\C-h" 'backward-delete-char)
  (setq search-delete-char 8)
  (define-key global-map "\177" 'delete-char)
  ;;M-DEL ?
  )
;(unless window-system (del))

;;
;; Global-map Key Bindings
;;

(defun subkey (prefix)
  (when (boundp `electric-history-map)
    (global-set-key (concat prefix prefix) 'electric-command-history)
    (define-key electric-history-map " " 'Electric-command-history-redo-expression))
  ;; C-\ shift-{symbol} or shift-{letter)... Toggles, relatively hard to type
  ;; C-\@ reserved for mark
  (global-set-key (concat prefix "~") 'toggle-read-only)	;; M-~ is not-modified
  (global-set-key (concat prefix "!") 'shell)		;; M-! is shell-command
  (global-set-key (concat prefix "$") 'ispell-buffer)	;; ESC-$ is ispell-word
  (global-set-key (concat prefix "E") 'toggle-debug-on-error)	;; (e)rror
  (global-set-key (concat prefix "C") 'toggle-case-fold-search)	;; (c)ase fold
  (global-set-key (concat prefix "T") 'toggle-truncate-lines)	;; (t)runcate lines
  (global-set-key (concat prefix "F") 'auto-fill-mode)		;; (f)ill
  (global-set-key (concat prefix "O") 'overwrite-mode)		;; (o)verwrite
  ;; doesn't change, seperate buffer
  (global-set-key (concat prefix "=") 'goto-line)		;; C-x = is what-position
  (global-set-key (concat prefix "c") 'compile)		;;(c)ompile
  (global-set-key (concat prefix "g") 'grep-defaulted)	;;(g)rep
  (global-set-key (concat prefix "o") 'occur-defaulted)	;;(o)ccur
  (global-set-key (concat prefix "p") 'lpr-buffer)		;;(p)rint
  (global-set-key (concat prefix "w") 'compare-windows)	;;(w)indows
  ;; Searching, etc, C-\C-{letter}
  (global-set-key (concat prefix "\C-d") 'delete-matching-lines)	;;(d)elete
  (global-set-key (concat prefix "\C-k") 'keep-lines)		;;(k)eep
  (global-set-key (concat prefix "\C-q") 'query-replace-regexp)	;;(q)uery
  (global-set-key (concat prefix "\C-r") 'replace-regexp)		;; C-r is search
  (global-set-key (concat prefix "\C-s") 'sort-lines)		;;(s)ort
  (global-set-key (concat prefix "\C-u") 'uniq-lines)		;;(u)niq
  ;; Mail inserters
  (global-set-key (concat prefix "ib") (lambda () (interactive) (mail-insert-file "~aseemm/Mail/amc_BEGINS")))
  (global-set-key (concat prefix "if") (lambda () (interactive) (mail-insert-file "~aseemm/Mail/amc_FILLED")))
  (global-set-key (concat prefix "ir") (lambda () (interactive) (mail-insert-file "~aseemm/Mail/amc_REGISTERED")))
  ;;(global-set-key (concat prefix "\C-t") 'crypt-timer)
  )

(progn
  (defvar Personal-map nil "The Personal-map maps the C-\\ personal keys.")
  (global-set-key "\M- " `set-mark-command)
  (global-set-key "\C-x\C-o" `read-mail-outlook)
  (global-set-key "\C-z" Personal-map)
  (global-set-key "\C-zz" (if window-system 'iconify-or-deiconify-frame 'suspend-emacs))
  (subkey "\C-z")
  (global-set-key "\C-\\" Personal-map)
  (global-set-key "\C-\\\\" 'toggle-input-method)
  (subkey "\C-\\"))

(progn
  ;; Change delete key & rebind help
  ;; (group-keys)
  ;;
  ;;(del)
  ;;
  ;; Make regexp search the default
  (define-key global-map "\C-s" 'isearch-forward-regexp)
  (define-key global-map "\C-r" 'isearch-backward-regexp)
  (define-key esc-map "\C-s" 'isearch-forward)
  (define-key esc-map "\C-r" 'isearch-backward)
  ;;
  (cond ((and mail-ok-p at-work-p (file-exists-p (concat (getenv "HOME") "/Mail/.outlook")))
	 (global-set-key "\C-x\C-m" 'read-mail-outlook))
	(mail-ok-p
	 (global-set-key "\C-x\C-m" 'rmail))
	(t
	 (global-set-key "\C-x\C-m" 'rmail-nope)))
  (global-set-key "\C-ha" 'apropos)	; not apropos-command
  ;;
  ;; Fix exit
  ;; (global-set-key "\C-x\C-c" 'kill-emacs-message)
  ;; (defun sbke () (interactive) (save-buffers-kill-emacs))
  ;;
  ;;(global-set-key [f2] (lambda () (interactive) (upcase-word -1)))
  (global-set-key [f1] 'my-buffer-prev)
  (global-set-key [f2] 'my-buffer-next)
  (global-set-key [f3] 'other-window)
  (global-set-key [f4] 'goto-line)
  (global-set-key [f5] (lambda () (interactive) (find-file "~/") (delete-other-windows) (goto-char 12000)))
  (global-set-key [f6] 'call-last-kbd-macro)
  (global-set-key [f7] 'save-buffer)
  (global-set-key [f8] 'save-some-buffers)
  (global-set-key [f9] 'font-lock-mode)
  (global-set-key [f16] 'my-haskell-load-and-run)
  (global-set-key [f17] (lambda () (interactive) (dired "~/")))
  )

;; (if emacs-type-xemacs
;;     (set-specifier default-toolbar-visible-p nil)
;;   (menu-bar-mode -1))

(when window-system
  ;;(set-default-font "-misc-fixed-medium-r-normal--18-*-*-*-c-70-iso8859-1")
  )

;; Remember this alternative to hooks:
;; (eval-after-load 'rmail '(progn (define-key rmail-mode-map "F" 'mime-forward)))

;; Finally, specific junk for WNT
(defun mail-pop ()
  (interactive)
  (mail-pop-from)
  (mail-pop-to)
  )

(defun mail-pop-to ()
  (interactive)
  (setq sendmail-program (expand-file-name "~/global/bin/ssh_sendmail"))
  ;(setq smtpmail-default-smtp-server "smtp.ma.ultranet.com")
  ;(setq smtpmail-default-smtp-server "smtp.rcn.com")
  ;(setq smtpmail-local-domain nil)
  ;(setq send-mail-function 'smtpmail-send-it)
  ;(load-library "smtpmail")
  )

(defun mail-pop-to-new ()
  (interactive)
  (setq smtpmail-default-smtp-server "world.std.com")
  (setq smtpmail-smtp-server "world.std.com")
  (setq smtpmail-auth-credentials
	'(("YOUR SMTP HOST" 25 "username" "password")))
  (setq smtpmail-debug-info t)
  (setq smtpmail-local-domain nil)
  (setq send-mail-function 'smtpmail-send-it)
  (load-library "smtpmail")
  )

(defun mail-pop-from ()
  (interactive)
  ;(setenv "MAILHOST" "pop.rcn.com")
  ;(setenv "MAILHOST" "pop.ma.ultranet.com")
  ;(setq rmail-primary-inbox-list '("po:aseemm.ma.ultranet" "/var/spool/mail/aseemm"))
  (setq rmail-pop-password-required t)
  (setq rmail-primary-inbox-list (list "/var/spool/mail/aseemm"
				       (concat (getenv "HOME") "/Mail/.spoolin")))
  )

(defun mail-pop-from-ultranet ()
  (interactive)
  (setenv "MAILHOST" "pop.rcn.com")
  (setq rmail-primary-inbox-list '("po:aseemm.ma.ultranet"))
  )

(defun mail-pop-from-2 ()
  (interactive)
  (setenv "MAILHOST" "mail.attbi.com")
  (setq rmail-primary-inbox-list '("po:aseemm_attbi" "/var/spool/mail/aseemm")
	rmail-pop-password-required t)
  )

(defun mail-insert-file (filename)
  (insert-file (expand-file-name filename)))

(defun read-mail-outlook ()
  (interactive)
  (find-file "~aseemm/Mail/.outlook"))

(defun bcc () (interactive) (insert (concat "BCC: " user-mail-address "\n")))

(defun mw ()
  (interactive)
  (mail-pop))

(defun home-attbi ()
  (interactive)
  (find-file "/aseemm_attbi@upload.attbi.net:/" 1))

(defun text ()
  (interactive)
  (text-mode)
  (widen)
  (toggle-read-only -1))

;; (when system-type-microsoft
;;   (setq dired-chmod-program "chmod")
;;   (setq explicit-shell-file-name "c:/bin/bash.exe")
;;   (setq shell-file-name explicit-shell-file-name)
;;   (setq grep-command "c:/bin/egrep.exe -n -r ")
;;   (setq grep-null-device "")
;;   )
(mail-pop)

(insert "\n")	;; So really don't get startup message in V20

(if (file-exists-p "c:/home/")
    (dired "~/"))

;; (when system-type-microsoft
;;   (setq  c-indent-level                4
;; 	 c-continued-statement-offset  4
;; 	 c-brace-offset               -5
;; 	 c-argdecl-indent              0
;; 	 c-label-offset               -5))

;;(defun my-shell-setup ()
;;  "For Cygwin bash under Emacs 20"
;;  (setq comint-scroll-show-maximum-output 'this)
;;  (setq comint-completion-addsuffix t)
;;  ;; (setq comint-process-echoes t) ;; reported that this is no longer needed
;;  (setq comint-eol-on-send t)
;;  (setq w32-quote-process-args ?\")
;;  (make-variable-buffer-local 'comint-completion-addsuffix))
;;
;;(setq shell-mode-hook 'my-shell-setup)

;(menu-bar-mode 1)
;(load-file "$SL/verilog-mode.el")
(setq-default truncate-lines t)

;; (when emacs-type-xemacs
;;   (custom-set-faces
;;    '(info-node ((t (:bold t))))
;;    '(hyper-apropos-section-heading ((t (:bold t))))
;;    '(message-cited-text ((t nil)))
;;    '(message-header-newsgroups-face ((((class color) (background light))
;; 				      (:foreground "blue4" :bold t)))))
;;   (setq *try-oblique-before-italic-fonts* t)
;;   )

;;EMACS-21:
;;*** The new user-option rmail-user-mail-address-regexp can be
;;*** The Ebrowse package implements a C++ class browser and tags
;;   M-x delimit-columns-region

;; CUSTOM
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(font-lock-builtin-face ((((class color) (background light)) (:foreground "VioletRed"))))
;;  '(font-lock-comment-face ((((class color) (background light)) (:foreground "Firebrick"))))
;;  '(font-lock-function-name-face ((((class color) (background light)) (:foreground "Blue"))))
;;  '(font-lock-keyword-face ((((class color) (background light)) (:foreground "Purple"))))
;;  '(font-lock-reference-face ((t (:foreground "MediumBlue"))))
;;  '(font-lock-string-face ((t (:foreground "Brown"))))
;;  '(font-lock-type-face ((t (:foreground "DarkGreen"))))
;;  '(font-lock-variable-name-face ((t (:foreground "Maroon"))))
;;  '(font-lock-warning-face ((t (:bold t :foreground "OrangeRed"))))
;;  '(fringe ((((class color) (background light)) (:background "#c0c0c0c0bbbb"))))
;;  '(hyper-apropos-section-heading ((t (:bold t))))
;;  '(info-node ((t (:bold t))))
;;  '(message-cited-text ((t nil)))
;;  '(message-header-newsgroups-face ((((class color) (background light)) (:foreground "blue4" :bold t))) t)
;;  '(sh-heredoc ((((class color) (background light)) (:foreground "tan4"))))
;;  '(shadow ((((class color) (min-colors 8) (background light)) (:foreground "blue1"))))
;;  '(subscript ((t nil)))
;;  '(comint-highlight-prompt ((t (:foreground "green"))))
;;  '(default ((t (:stipple nil :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 94 :width normal :family "misc-fixed"))))
;;  '(Plum1 ((t (:stipple nil :foreground "Plum1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 94 :width normal :family "misc-fixed"))))
;;  '(border ((t (:background "blue"))))
;;  '(comint-highlight-input ((t (:foreground "white" :weight bold))))
;;  '(comint-highlight-prompt ((t (:foreground "green"))))
;;  '(cursor ((t (:background "green"))))
;;  '(custom-comment-face ((((class grayscale color) (background dark)) (:background "blue"))))
;;  '(cyan ((t (:stipple nil :foreground "cyan" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 94 :width normal :family "misc-fixed"))))
;;  '(cyan-bold ((t (:stipple nil :foreground "cyan" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 94 :width normal :family "misc-fixed"))))
;;  '(fringe ((t nil)))
;;  '(green ((t (:stipple nil :foreground "green" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :family "adobe-courier"))))
;;  '(isearch ((t (:background "pink3" :foreground "white"))))
;;  '(isearch-lazy-highlight-face ((t (:background "magenta4"))))
;;  '(moccasin ((t (:stipple nil :foreground "moccasin" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :family "adobe-courier"))))
;;  '(mouse ((t (:background "black"))))
;;  '(orange ((t (:stipple nil :foreground "orange" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 94 :width normal :family "misc-fixed"))))
;;  '(region ((t (:background "green" :foreground "White"))))
;;  '(rpm-spec-tag-face ((((class color) (background dark)) (:foreground "green"))))
;;  '(tomato ((t (:stipple nil :foreground "tomato" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 94 :width normal :family "misc-fixed"))))
;;  '(yellow ((t (:stipple nil :foreground "yellow" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 94 :width normal :family "misc-fixed"))))
;;  )

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(load-home-init-file t t)
 '(safe-local-variable-values (quote ((c-brace-offset . -4) (c-argdecl-indent . 4) (c-label-offset . -4) (c-continued-statement-offset . 4) (TeX-master . "icet") (checkdoc-permit-comma-termination-flag . t) (checkdoc-force-docstrings-flag))))
 '(scroll-bar-mode (quote right)))
(setq load-home-init-file t) ; don't load init file from ~/.xemacs/init.el

(put 'scroll-left 'disabled nil)
(setq load-home-init-file t) ; don't load init file from ~/.xemacs/init.el

(fset 'yes-or-no-p 'y-or-n-p) ; Use "y or n" answers instead of full words "yes or no"
(ansi-color-for-comint-mode-on) ; interpret and use ansi color codes in shell output windows   
(require 'paren) ; load paren highlighting

(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
;; (color-theme-calm-forest)
;; (color-theme-dark-laptop)
;; (color-theme-charcoal-black)
;; (color-theme-comidia)
(color-theme-billw)

;; (load-file "~/elisp/color-theme-solarized.el")
;; (color-theme-solarized-dark)
;; (set-face-background `modeline "#626262")

;; don't create backup files
(setq make-backup-files nil)

;; webmode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; haskell mode
(load "~/elisp/haskell-mode-2.8.0/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(defun my-haskell-load-and-run ()
  "Loads and runs the current Haskell file."
  (interactive)
  (let ((start-buffer (current-buffer)))
    (inferior-haskell-load-and-run inferior-haskell-run-command)
    (sleep-for 0 100)
    (end-of-buffer)
    (pop-to-buffer start-buffer)))


