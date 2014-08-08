;; vtest-mode.el --- major mode for editing vtest files
;;
;; $Id: vtest-mode.el,v 3.5 2006/09/20 13:21:46 wsnyder Exp $

;; Author          : Wilson Snyder <wsnyder@wsnyder.org>
;; Keywords        : languages

;;; Commentary:
;;
;; Distributed from the web
;;	http://www.veripool.com
;;
;; To use this package, simply put it in a file called "vtest-mode.el" in
;; a Lisp directory known to Emacs (see `load-path'), byte-compile it
;; and put the lines (excluding START and END lines):
;;
;;	---INSTALLER-SITE-START---
;;	;; Vtest-mode
;;	(autoload 'vtest-mode "vtest-mode" "Mode for Vtest Makefiles." t)
;;	(setq auto-mode-alist (append (list '("\\.vtest$" . vtest-mode)) auto-mode-alist))
;;	---INSTALLER-SITE-END---
;;
;; in your ~/.emacs file or in the file site-start.el in the site-lisp
;; directory of the Emacs distribution.
;;
;; If you do not wish to bind all .log files to this mode, then make sure the
;; last lines of your log files contain:
;;     ;;; Local Variables: ***
;;     ;;; mode:vtest ***
;;     ;;; End: ***

;; COPYING:
;;
;; vtest-mode.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; vtest-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with vtest; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; History:
;; 


;;; Code:

(require 'font-lock)
(provide 'vtest-mode)

(defconst vtest-version "$$Revision: 3.5 $$"
  "Version of this vtest mode.")

;;
;; Global Variables, user configurable
;;

(defgroup vtest nil
  "Vtest file editing"
  :group 'languages)

(defcustom vtest-mode-hook nil
  "*Hook (List of functions) run after vtest mode is loaded."
  :type 'hook
  :group 'vtest)

;;
;; Bindings
;;

(defvar vtest-mode-map ()
  "Keymap used in Vtest mode.")
(if vtest-mode-map
    ()
  (setq vtest-mode-map (make-sparse-keymap))
  ;; No keys yet - Amazing isn't it?
  )

;;
;; Menus
;;

;; Not until we have keys!

;;
;; Internal Global Variables
;;

;;;
;; Font-Lock
;;

(defun vtest-font-lock-keywords ()
  "Return the keywords to be used for font-lock."
  `(
    ("#.*$" 0 'font-lock-comment-face t)		; red
    ("^\\s *=[A-Za-z0-9_]+" 0 'font-lock-builtin-face)	; pink
    ;; {..} expansion
    ("{[A-Za-z0-9_.,]+}/"  0 'font-lock-keyword-face)	; purple (units)
    ("{[A-Za-z0-9_.,]+}"  0 'font-lock-type-face)	; green (targets/vars)
    ;; ,param
    (",[A-Za-z0-9_]+" 0 'font-lock-variable-name-face)	; red
    ("~[A-Za-z0-9_]+" 0 'font-lock-string-face)		; red (like comment)
    ;; =value
    ("==0" 0 'font-lock-warning-face)		; Disable - dark red
    ("=[A-Za-z0-9---+_.]+" 0 'font-lock-constant-face)	; cyan
    ;; Unit/Test
    ("\\([A-Za-z0-9_]*/\\)\\([A-Za-z0-9_]+\\)"
     (1 font-lock-keyword-face)	; purple
     (2 font-lock-function-name-face)) ; blue
    ;; Unit/Alias
    ("\\([A-Za-z0-9_]*/\\)\\([@][A-Za-z0-9_]+\\)"
     (1 font-lock-keyword-face)	; purple
     (2 font-lock-type-face)) 	; green
    ;; alias
    ("\\(@?[A-Za-z0-9_]+\\)" 0 'font-lock-type-face)	; green
    ))
	

;;;
;; Mode
;;

(defun vtest-mode ()
  "Major mode for editing Vtest Synopsys Constraint files.

Turning on Vtest mode calls the value of the variable `vtest-mode-hook'
with no args, if that value is non-nil.

Special commands:\\{vtest-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map vtest-mode-map)
  (setq major-mode 'vtest-mode)
  (setq mode-name "Vtest")
  ;;
  ;; Font lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults  '((vtest-font-lock-keywords)
			      nil nil nil beginning-of-line))
  ;;
  (run-hooks 'vtest-mode-hook))


;;
;; Installing, other utilities
;;

(defun vtest-install ()
  "Install vtest.  Only required for initial installation from distribution."
  (if (file-exists-p "installer.el")
      (load (expand-file-name "installer.el"))
    (require `installer))
  (installer-add-file "vtest-mode.el"))


(provide 'vtest-mode)

;;; vtest-mode.el ends here
