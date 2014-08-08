;;; $Id$
;;; These are general mail features, see also rmail-add for rmail specifics

(provide 'sendmail-add)
(require 'sendmail)

;;;;
;;;; Defines
;;;;

(defvar sendmailadd-force-work nil
  "Positive to force next message to be sent to use work address,
Negative for internal, nil for auto-choosing.")

;;;;
;;;; Keys
;;;;

(define-key mail-mode-map "\t"	'indent-for-tab-command)
(define-key mail-mode-map "\C-c\C-f\C-p"	'sendmailadd-insert-phone)
(define-key mail-mode-map "\C-c\C-f\C-e"	'from-e)
(define-key mail-mode-map "\C-c\C-f\C-w"	'from-w)

(defun bcc () (interactive) (insert "BCC: wsnyder@wsnyder.org\n"))
(defun from-e () (interactive)
  (setq user-mail-address user-mail-address-ext)
  (sendmailadd-insert-from))
(defun from-w () (interactive)
  (setq user-mail-address user-mail-address-work)
  (sendmailadd-insert-from))

;;;;
;;;; Sending
;;;;

(defun sendmailadd-insert-phone (regexp)
  (interactive "sPhone regexp: ")
  (save-excursion
    ;(goto-char (point-min))
    ;(re-search-forward "^To: [^\n]*\n" nil t)
    ;(backward-char 1)
    (let ((s (shell-command-to-string (concat "phone --email " regexp))))
      (while (string-match "\n+" s)
	(setq s (replace-match ", " nil nil s)))
      (insert s))))

(defun sendmailadd-insert-from ()
  (save-excursion
    (if (re-search-backward "^From: [^\n]*\n" nil t)
	(delete-region (match-beginning 0) (match-end 0)))
    (insert "From: \"" user-full-name "\" <" user-mail-address ">\n")))

(defun sendmailadd-insert-from-auto ()
  (save-excursion
    (goto-char (point-min))
    ;;(set-user-mail-address-int-ext)
    (insert "From: \"" user-full-name "\" <" user-mail-address ">\n")
    ))
;;Double from in Emacs23: (add-hook 'mail-setup-hook 'sendmailadd-insert-from-auto)

(defun sendmailadd-from-work-p ()
  "Return 1 if from work, 0 to non work, nil undeterminate"
  (save-excursion
    (goto-char (point-min))
    (let ((to-work nil)
	  (to-ext nil)
	  (to (progn (re-search-forward "To:\\([^\n]*\\)" nil t)
		     (match-string 1)))
	  domain)
      (while (string-match "@\\([a-z---A-Z0-9._]+\\)\\(.*\\)$" to)
	(setq domain (match-string 1 to)
	      to (match-string 2 to))
	(if (string-match "\\<\\(sicortex\\).com" domain)
	    (setq to-work t)
	  (setq to-ext t)))
      (cond ((and to-work (not to-ext))
	     1)
	    ((and (not to-work) to-ext)
	     0)
	    nil)
      )))

(defun set-user-mail-address-int-ext ()
  (setq user-mail-address
	(if (> (or sendmailadd-force-work
		   (sendmailadd-from-work-p)
		   0)
	       0)
	    user-mail-address-work
	  user-mail-address-ext))
  ;(setq sendmailadd-force-work nil)
  user-mail-address)

;; No longer used, separate mail systems
;; See also sendmailadd-insert-from-auto
;;(add-hook 'mail-send-hook 'set-user-mail-address-int-ext)

