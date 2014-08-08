;; vregs-mode.el --- minor mode for displaying waveform signal values
;;
;; $Revision: #8 $$Date: 2004/01/27 $$Author: wsnyder $

;; Author          : Wilson Snyder <wsnyder@wsnyder.org>
;; Keywords        : languages

;;; Commentary:
;; 
;; Distributed from the web
;;	http://www.veripool.com
;;
;; To use this package, simply put it in a file called "vregs-mode.el" in
;; a Lisp directory known to Emacs (see `load-path').
;;
;; Byte-compile the file (in the vregs-mode.el buffer, enter dired with C-x d
;; then press B yes RETURN)
;;
;; Put these lines in your ~/.emacs or site's site-start.el file (excluding
;; the START and END lines):
;;
;;	---INSTALLER-SITE-START---
;;	;; Vregs mode
;;     (autoload 'vregs-mode "vregs-mode" "Mode for vregs files." t)
;;	(setq auto-mode-alist (append (list '("\\.rpt[^.]*$" . vregs-mode)) auto-mode-alist))
;;	---INSTALLER-SITE-END---

;; COPYING:
;;
;; Copyright 2001-2004 by Wilson Snyder.  This program is free software;
;; you can redistribute it and/or modify it under the terms of either the GNU
;; General Public License or the Perl Artistic License.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the Perl Artistic License
;; along with this module; see the file COPYING.  If not, see
;; www.cpan.org
;; 

;;; History:
;; 


;;; Code:

(provide 'vregs-mode)

(defvar vregs-mode-abbrev-table nil
  "Abbrev table in use in `vregs-mode' buffers.")
(define-abbrev-table 'vregs-mode-abbrev-table ())

(defvar vregs-mode-hook nil
  "Run at the very end of `vregs-mode'.")

(defvar vregs-mode-map ()
  "Keymap used in Vregs mode.")
(unless vregs-mode-map
  (setq vregs-mode-map (make-sparse-keymap))

  ;; Make menu bar items.
  (define-key vregs-mode-map [menu-bar Vregs]
    (cons "Vregs" (make-sparse-keymap "Vregs")))
  )

;;;;========================================================================
;;;; Highlighting

;; Nothing yet
(defconst vregs-mode-font-lock-keywords `(t
    ;;("@\\(cite\\|xref\\|pxref\\){\\([^}]+\\)"
    ;; (2 font-lock-reference-face))
    ;;("@\\(end\\|itemx?\\) +\\(.+\\)"
    ;; (2 font-lock-function-name-face keep))
    ))
(defun vregs-mode-font-lock-keywords ()
  "Return the keywords to be used for font-lock."
  (list (list "//.*$"
	      0 font-lock-comment-face) ; Redish
	;; Fontify function macro names.
	(list "^[ \t]*#[ \t]*define[ \t]+\\(\\sw+\\)("
	      1 font-lock-function-name-face)
	;; Fontify symbol names in #elif or #if ... defined preprocessor directives.
	(list "^[ \t]*#[ \t]*\\(elif\\|if\\)\\>"
	      (list "\\<\\(defined\\)\\>[ \t]*(?\\([a-zA-Z0-9_$]+\\)?" nil nil
		    (list 1 font-lock-builtin-face)
		    (list 2 font-lock-variable-name-face nil t)))
	;; Fontify otherwise as symbol names, and the preprocessor directive names.
	(list "^[ \t]*#[ \t]*\\(ifdef\\|define\\|undef\\|else\\|endif\\|include\\|ident\\)\\>[ \t!]*\\([a-zA-Z0-9_$]+\\)?"
	      (list 1 font-lock-builtin-face)
	      (list 2 font-lock-variable-name-face nil t))
	;; Else presume is a old style comment
	(list "#.*$"
	      0 font-lock-comment-face)
	(list "^\\s *\\<\\(bit\\|const\\|define\\)\\>\\s *\\([a-zA-Z0-9_]+\\)"
	      (list 1 font-lock-keyword-face)
	      (list 2 font-lock-variable-name-face))
	(list "^\\s *\\<\\(reg\\|cfg\\)\\>\\s *\\([a-zA-Z0-9_*]+\\)\\s *\\([a-zA-Z0-9_*]+\\)"
	      (list 1 font-lock-keyword-face)
	      (list 2 font-lock-variable-name-face)
	      (list 3 font-lock-type-face))
	(list "^\\s *\\<\\(type\\|enum\\)\\>\\s *\\([a-zA-Z0-9_*]+\\)"
	      (list 1 font-lock-keyword-face)
	      (list 2 font-lock-type-face))
	(list "^\\s *\\<\\(use\\|init\\|end\\|checks\\|package\\)\\>"
	      1 font-lock-keyword-face)
	(list "\\<\\([RC]_[A-Z0-9_]+\\)\\s *="
	      1 font-lock-variable-name-face)
	))
	
;;;;
;;;; Mode stuff
;;;;


(defun vregs-mode ()
  "Major mode for editing Vregs results.

Turning on Vregs mode calls the value of the variable `vregs-mode-hook'
with no args, if that value is non-nil.

Special commands:\\{vregs-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map vregs-mode-map)
  (setq major-mode 'vregs-mode)
  (setq mode-name "Vregs")
  (setq local-abbrev-table vregs-mode-abbrev-table)
  ;;
  ;; Font lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults  '(vregs-mode-font-lock-keywords
			      nil nil nil beginning-of-line))
  ;;
  ;;(make-local-variable 'indent-line-function)
  ;;(setq indent-line-function 'vregs-indent-line)
  ;;
  (run-hooks 'vregs-mode-hook))

(provide 'vregs-mode)

;;; vregs-mode.el ends here
