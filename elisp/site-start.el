;;; site-start.el --- Group wide startup file for GNU Emacs
;;;  by wsnyder@wsnyder.org
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

(setq site-start-version "$Id: site-start.el 77533 2009-05-19 20:08:33Z wsnyder $")
;;;======================================================================
;; System Types

(defvar system-type-unix      (not (eq system-type `windows-nt))
  "True if a Unix system")
(defvar system-type-microsoft (eq system-type `windows-nt)
  "True if a NT/Windows system")
(defvar emacs-type-xemacs     (string-match "XEmacs" emacs-version)
  "True if xemacs flavor")
(defvar emacs-type-21         (eq emacs-major-version 21)
  "True if gnu emacs 21 flavor")
(defvar location-type-svaha   (file-exists-p "/svaha")
  "True if on Svaha")
(defvar location-type-work    (and system-type-unix (not location-type-svaha))
  "True if on work site domain")

;;;======================================================================

;; load-path
(setq load-path (append (list (expand-file-name "~/lisp/")
			      (expand-file-name "~/site-lisp/")
			      (expand-file-name "~/site-lisp/local/")
			      ;;(expand-file-name "/PREFIX/noarch/emacs-site-lisp/contrib")
			      ;;(expand-file-name "/PREFIX/noarch/emacs-site-lisp/local")
			      )
			load-path))

;; 
;; We have lots of DRAM; Postpone garbage-collection
(setq gc-cons-threshold 10000000)

;; Set mail name
(setq your-email-name (getenv "USER")
      your-email-site "sicortex.com")

;;
;; C++-mode
(setq auto-mode-alist (append (list '("\\.C$" . c-mode) '("\\.m$" . c-mode)
				    '("\\.CXX$" . c++-mode) '("\\.cxx$" . c++-mode)
				    '("\\.HXX$" . c++-mode) '("\\.hxx$" . c++-mode)
				    '("\\.sig$" . c++-mode)
				    ) auto-mode-alist))
;;
;; Verilog-mode
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
(setq auto-mode-alist (append (list '("\\.vh?$" . verilog-mode)
				    '("\\.vc$"  . verilog-mode)
				    '("\\.vpp$" . verilog-mode)
				    '("\\.vpm$" . verilog-mode)
				    '("\\.svh?$" . verilog-mode)
				    ) auto-mode-alist))

(autoload 'dir-project-verilog-getopt "dir-project" "Expand project directories")
(add-hook 'verilog-getopt-flags-hook 'dir-project-verilog-getopt)

(defvar verilog-vppp-command  "vppreproc"
  "*Verilog preprocessor command for verilog-vppp")
(defvar verilog-auto-reset-widths t
  "*If true, AUTORESET should determine the width of signals")
(defun verilog-vppp (&optional filename)
  (interactive)
  (if (not filename)
      (setq filename (buffer-file-name)))
  (let* ((dir (file-name-directory filename))
	 (file (file-name-nondirectory filename))
	 (cmd (concat "cd " dir "; " verilog-vppp-command " " file)))
    (with-output-to-temp-buffer "*Vppp*"
      (save-excursion
	(set-buffer "*Vppp*")
	(insert (concat cmd "\n"))
	(shell-command cmd "*Vppp*")
	(verilog-mode)
	(font-lock-mode)
	))))

(add-hook 'verilog-mode-hook
	  '(lambda ()
	     (define-key verilog-mode-map "\C-c\C-p" 'verilog-vppp)))

;;
;; SystemC-mode
(autoload 'systemc-mode "systemc-mode" "SystemC mode" t )
(setq auto-mode-alist (append (list '("\\.sp$" . systemc-mode)
				    ) auto-mode-alist))

;;
;; Html mode
(autoload 'html-mode "html-mode" "HTML major mode." t)
(setq auto-mode-alist (append (list '("\\.html$" . html-mode)
				    '("\\.htmlpp$" . html-mode)
				    '("\\.htp$" . html-mode)
				    ) auto-mode-alist))
;; HTML should't destroy the group keybindings, just use a few features.
(progn
  (setq html-mode-map (make-sparse-keymap))
  (define-key html-mode-map "\C-cz" 'html-preview-document))

;; W3m
;;(require 'w3m-load)

;; Vtest mode
(autoload 'vtest-mode "vtest-mode" "Vtest Major Mode." t)
(setq auto-mode-alist (append (list '("\\.vtest$" . vtest-mode)) auto-mode-alist))

;; Vregs mode
(autoload 'vregs-mode "vregs-mode" "Vregs Major Mode." t)
(setq auto-mode-alist (append (list '("\\.vregs$" . vregs-mode)) auto-mode-alist))

;; Sim-Log mode
(autoload 'sim-log-mode "sim-log" "Mode for Simulation Log files." t)
(setq auto-mode-alist (append (list '("\\.log$" . sim-log-mode)) auto-mode-alist))

;; Boa-mode
(autoload 'boa-mode "boa-mode" "Mode for Boa Synopsys constraint files." t)
(setq auto-mode-alist (append (list '("\\.boa$" . boa-mode)) auto-mode-alist))

;; Synopsys mode
(autoload 'synopsys-mode "synopsys-mode" "Mode for synopsys output files." t)
(setq auto-mode-alist (append (list '("\\.rpt[^.]*$" . synopsys-mode)) auto-mode-alist))
(setq auto-mode-alist (append (list '("\\.scr[^.]*$" . tcl-mode)) auto-mode-alist))
(setq auto-mode-alist (append (list '("\\.sdc[^.]*$" . tcl-mode)) auto-mode-alist))

;; Apollo Mode
(autoload 'apollo-mode "apollo-mode" "Mode for Apollo output files." t)
(setq auto-mode-alist (append (list '("\\.apollo_rpt[^.]*$" . apollo-mode)) auto-mode-alist))

;; Einstimer mode
(autoload 'einstimer-mode "einstimer-mode" "Mode for Einstimer output files." t)
(setq auto-mode-alist (append (list '("\\..*_endpoint[^.]*$" . einstimer-mode)) auto-mode-alist))

;; Xilinx mode
(autoload 'xilinx-mode "xilinx-mode" "Mode for Xilinx output files." t)
(setq auto-mode-alist (append (list '("\\.twr$" . xilinx-mode)) auto-mode-alist))

;; Cadence RC mode
(autoload 'cadence-rc-mode "cadence-rc-mode" "Mode for Cadence RC output files." t)

;; Checkdoc
(put 'checkdoc-permit-comma-termination-flag 'safe-local-variable 'booleanp)
(put 'checkdoc-force-docstrings-flag 'safe-local-variable 'booleanp)

;; Perl mode
(setq auto-mode-alist (append (list '("sim_config$" . perl-mode)
				    '("\\.sim$" . perl-mode)
				    ) auto-mode-alist))

;; Python mode
(setq auto-mode-alist (append (list '("\\.py$" . python-mode)
				    ) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
				   interpreter-mode-alist))

;; Ispell mode
; Aspell is a replacement for ispell
(setq-default ispell-program-name "aspell") 

;;;END MODES====================

;(load "psgml-init" nil t) ;; psgml-auto-setup-1.2.1-8

;; Auto unziping
;(require 'jka-compr)
(when emacs-type-21
  (auto-compression-mode 1))

;; Compile
(add-hook 'compilation-mode-hook
	  '(lambda ()
	     (setq truncate-lines nil)))

;;
;; Verilog defaults
(autoload 'dinotrace-update "dinotrace" "Add dinotrace annotations to buffers." t)
(global-set-key "\C-x\C-aa" 'dinotrace-update)
(global-set-key "\C-x\C-ad" 'dinotrace-mode)
(setq verilog-linter "vlint --brief --andsynth ")
(put 'compile-command2 'safe-local-variable 'stringp)

(add-hook 'verilog-mode-hook
	  '(lambda () 
	     (abbrev-mode 0)
	     (make-local-variable 'compile-command)
	     (setq verilog-auto-newline nil
		   verilog-tab-always-indent nil
		   verilog-auto-endcomments nil
		   ;verilog-linter "vlint --brief "
		   verilog-compiler verilog-linter
		   verilog-user-keywords `("`systemc_clock")
		   verilog-auto-lineup 'declarations
		   fill-column 80
		   compile-command (concat verilog-compiler
					   (file-name-nondirectory (or buffer-file-name "")))
		   )
	     (visit-tags-table-if-exists (expand-file-name "./TAGS"))
	     (visit-tags-table-if-exists (expand-file-name "../TAGS"))
	     ))

;; Grep
(setq grep-command "egrep -n -r "
      grep-null-device "/dev/null")
(when (= emacs-major-version 22)	;; Future versions MIGHT fix:
  (setq grep-highlight-matches nil))	;; So slow on large lists as to be useless

;; C-Mode stuff
;;Put into your .emacs
;;(group-c-mode)
(defvar personal-tab-indent nil "Set this for the always-indent setting you like")
(if emacs-type-xemacs
  (defvar c++-font-lock-extra-types `())
  )
(defun group-c-mode (&optional style)
  (require 'font-lock)
  (with-temp-buffer
    (c-mode)
    (c-set-style (or style "cc-mode"))
    (setq-default c-auto-newline nil)
    (setq-default c-tab-always-indent personal-tab-indent)
    ;; Captial with some lower case is a type name
    ;; Don't anchor with ^ or $; the whole string must match
    (let ((extra-types `("\\(R_\\)?[A-Z][A-Z0-9]*[a-z][A-Za-z_0-9]*"
			 "V_[A-Za-z0-9_]*"
			 "T")))
      (setq c++-font-lock-extra-types (append extra-types c++-font-lock-extra-types))
      (setq c-font-lock-extra-types (append extra-types c++-font-lock-extra-types)))
    (add-hook 'c-mode-common-hook '(lambda () 
				     (c-set-style "cc-mode")
				     ;;(setq c-keywords (replace-regexp-in-string "const" "const\\|restrict" c-keywords))
				     (setq-default c-auto-newline nil)
				     (setq-default c-tab-always-indent personal-tab-indent)))))

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))
(setq auto-mode-alist (cons '("/usr/src/linux.*/.*\\.[ch]$" . linux-c-mode)
			    auto-mode-alist))

;; Gnats
;;
;(load "send-pr")	;; "gnats.el" has a dependency on "send-pr"
;(load "gnats")

;; GIT
;;Sources from kits/sources/git-*/contrib/emacs added into site_lisp
(add-to-list 'vc-handled-backends 'GIT)

;; CVS
(autoload 'cvs-update "pcl-cvs" nil t)
(autoload 'granny "granny" nil t)

;; Subversion
(unless emacs-type-xemacs
  (add-to-list 'vc-handled-backends 'SVN))

;; TeX
(require 'tex-mode)  ;; Sorry; need to override below, and latex-mode-hook is too late.
(defvar TeX-master nil "")
(put 'TeX-master 'safe-local-variable (lambda (x) (or (stringp x) (booleanp x))))
;; Bug in Emacs 22.1 causes \end{verbatim} to be missed, making a mess
(setq tex-verbatim-environments (list "verbatim_now_off"))

;; Disable _{subscript} as we're using underline package.
(defun tex-font-lock-match-suscript (limit) nil)

;; DIRED mode
(add-hook 'dired-mode-hook
	  (function
	   (lambda ()
	     (setq dired-copy-preserve-time t
		   case-fold-search t
		   dired-dwim-target t
		   truncate-lines t)
	     )))

(add-hook 'dired-load-hook
	  (function (lambda ()
		      (load "dired-add"))))

;; Name multiple buffers intelligently
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Windowing
;;(setq default-frame-alist (append (list (cons 'name (system-name)))
;;				  default-frame-alist))

;; History
(unless noninteractive
  (load "echistory"))	;; C-c C-c for electric command history
(unless emacs-type-xemacs
  (show-paren-mode t))

;; Minibuffer
;; Minibuffer shouldn't truncate so we can see what goes on
(add-hook 'minibuffer-setup-hook 
	  (function (lambda ()
		      (setq truncate-lines nil))))

;;
;; Remove .log's from the emacs default; want to complete them all the time
(delete ".log" completion-ignored-extensions)

;;
;; Shell stuff
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

;;
;; Misc config stuff
(load "mod-funcs" nil t)
(load "rect-add" nil t)

;; Change next-line to not add CR's
(setq-default next-line-add-newlines nil)

;; Allow variables to be set (for .v file AUTOs)
(setq enable-local-eval t)

;; Don't ask about visiting tags tables
(setq tags-add-tables t)

;; Turn off blink (better for slow links)
;;(blink-cursor-mode 0)

;; Autosave & Backup stuff
(setq-default make-backup-files nil)

;; Disable stupid warnings
(setq warning-suppress-types '((undo discard-info)))

;; Version control
(when emacs-type-xemacs
  (require 'vc-hooks)
  )

;; Put this in your .emacs to disable the C-x\C-c combo:
;;(global-set-key "\C-x\C-c" 'kill-emacs-message)

;; Typing "yes" all of the time is very annoying
;; To reset: (fset 'yes-or-no-p 'old-yes-or-no-p)
(fset 'old-yes-or-no-p (symbol-function 'yes-or-no-p))
(fset 'yes-or-no-p 'y-or-n-p)

;;
;; Global-map Key Bindings
(defun group-keys ()
  "Setup group key bindings - at least the more general ones."
  (interactive)
  ;; Turn off F1 help definition
  (setq key-translation-map nil)
  ;; X11 Stuff
  (global-set-key [f11] 'ESC-prefix)
  ;;
  ;; ESC ESC is evaluate, backwards emacs compatibility
  (global-set-key "\e\C-x" 'eval-defun)	; global, not just emacs-lisp mode
  (global-set-key "\e\e" 'eval-expression)
  ;;
  ;; May be overridden in mode specifics
  (global-set-key "\C-c\C-f" 'find-file-at-point)
  (global-set-key "\C-c\C-s" 'save-and-compile)
  (global-set-key "\C-c\C-u" 'browse-url-at-point)
  ;;
  ;; Rectangle command map
  (global-set-key "\C-xrr" 'sort-columns-untabify)
  (global-set-key "\C-xrv" 'yank-tilted-rectangle)
  (global-set-key "\C-xrz" 'close-rectangle)
  (global-set-key "\C-xr+" 'sum-rectangle)
  (global-set-key "\C-xr=" 'sum-rectangle)
  (global-set-key "\C-xr\t" 'tab-rectangle)
  (global-set-key "\C-xr\C-t" 'string-rectangle)
  (global-set-key "\C-xr#" 'numerate-rectangle)
  )

;;
;; For fancy colors, put in .emacs:
;;(global-font-lock-mode t)
