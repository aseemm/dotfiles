;;; $Id$
;;; These are rmail features, see also mail-add for general mail

(require 'rmail)
(require 'mail-add)

;;(require 'rmailsum)
(if (not (boundp 'rmail-summary-font-lock-keywords))
    (load "rmailsum"))

(provide 'rmail-add)

(defun rmail-make-in-reply-to-field (from date message-id)
  ;;"Currently NIL as don't care about in-reply to."
  nil
  )

;;;;
;;;; Keys
;;;;

;;(define-key rmail-mode-map "\C-cp"      'rmail-lpr-message)
(define-key rmail-mode-map "q"	'rmail-quit-expunge)
(define-key rmail-mode-map "z"	'crypt-rmail-decrypt)
(define-key rmail-mode-map "M"	'rmail-mime-decrypt)
(define-key rmail-mode-map "O"	'rmail-mime-output)
(define-key rmail-mode-map "W"	'rmail-web-output)
(define-key rmail-mode-map "D"	'rmail-outlook-date)
(define-key rmail-mode-map "Q"	'rmail-quit-expunge)
(define-key rmail-mode-map "B"	'mail-bounce)
(define-key rmail-mode-map "j"	'mail-junk)
(define-key rmail-mode-map "F"	'mime-forward)
(define-key rmail-mode-map "K"	'kill-label-detached)

(define-key rmail-summary-mode-map "q"	'rmail-quit-expunge)
(define-key rmail-summary-mode-map "Q"	'rmail-quit-expunge)
(define-key rmail-summary-mode-map "F"	'mime-forward)
(define-key rmail-summary-mode-map "K"	'kill-label-detached)


(autoload 'crypt-rmail-decrypt "crypt++-add" "Mode for editing booklist." t)

;;;;
;;;; Stolen code
;;;;

(defun rmail-from-at-point ()
  "From rmail-make-basic-summary-line"
  (save-excursion
    (let* ((from (mail-strip-quoted-names
		  (buffer-substring
		   (1- (point))
		   ;; Get all the lines of the From field
		   ;; so that we get a whole comment if there is one,
		   ;; so that mail-strip-quoted-names can discard it.
		   (let ((opoint (point)))
		     (while (progn (forward-line 1)
				   (looking-at "[ \t]")))
		     ;; Back up over newline, then trailing spaces or tabs
		     (forward-char -1)
		     (skip-chars-backward " \t")
		     (point))))))
      from)))

;;;;
;;;; Patches
;;;;

;;rmailsum.el
(defvar rmail-from-myself-regexp "wsnyder\\|snyder\\|snydew2\\|wilson\\.snyder")

;; Include year
(defun rmail-make-basic-summary-line () "WPS FIXED"
  (goto-char (point-min))
  (concat (save-excursion
	    ;; WPS FIX to include year in date
	    (if (not (re-search-forward "^Date:" nil t))
		"         "
	      ;;                         Date: Mon,           22                              Jul 1996 12:10:31 -0400
	      (cond ((re-search-forward "\\([^0-9:]\\)\\([0-3]?[0-9]\\)\\([- \t_]+\\)\\([adfjmnos][aceopu][bcglnprtvy]\\)[- \t_]*\\([12][90]\\|\\)\\([0-9][0-9]\\)"
		      (save-excursion (end-of-line) (point)) t)
		     (format "%2d-%3s-%2s"
			     (string-to-int (buffer-substring
					     (match-beginning 2)
					     (match-end 2)))
			     (buffer-substring
			      (match-beginning 4) (match-end 4))
			     (buffer-substring
			      (match-beginning 6) (match-end 6))))
		    ((re-search-forward "\\([^a-z]\\)\\([adfjmnos][acepou][bcglnprtvy]\\)\\([-a-z \t_]*\\)\\([0-9][0-9]?\\)[-a-z \t_,]*\\([12][90]\\|\\)\\([0-9][0-9]\\)"
		      (save-excursion (end-of-line) (point)) t)
		     (format "%2d-%3s-%2s"
			     (string-to-int (buffer-substring
					     (match-beginning 4)
					     (match-end 4)))
			     (buffer-substring
			      (match-beginning 2) (match-end 2))
			     (buffer-substring
			      (match-beginning 6) (match-end 6))))
		    (t "?????????"))))
	  "  "
	  (save-excursion
	    (if (not (re-search-forward "^From:[ \t]*" nil t))
		"                         "
	      (let* ((from (mail-strip-quoted-names
			    (buffer-substring
			     (1- (point))
			     ;; Get all the lines of the From field
			     ;; so that we get a whole comment if there is one,
			     ;; so that mail-strip-quoted-names can discard it.
			     (let ((opoint (point)))
			       (while (progn (forward-line 1)
					     (looking-at "[ \t]")))
			       ;; Back up over newline, then trailing spaces or tabs
			       (forward-char -1)
			       (skip-chars-backward " \t")
			       (point)))))
                     len mch lo)
		(if (string-match (concat "^\\("
					  (regexp-quote (user-login-name))
					  (if rmail-from-myself-regexp
					      (concat "\\|" rmail-from-myself-regexp) "")
					  "\\($\\|@\\)\\|"
					  (regexp-quote
					   ;; Don't lose if run from init file
					   ;; where user-mail-address is not
					   ;; set yet.
					   (or user-mail-address
					       (concat (user-login-name) "@"
						       (or mail-host-address
							   (system-name)))))
					  "\\>\\)")
				  from)
		    (save-excursion
		      (goto-char (point-min))
		      (if (not (re-search-forward "^To:[ \t]*" nil t))
			  nil
			(setq from
			      (concat "to: "
				      (mail-strip-quoted-names
				       (buffer-substring
					(point)
					(progn (end-of-line)
					       (skip-chars-backward " \t")
					       (point)))))))))
		(setq len (length from))
		(setq mch (string-match "[@%]" from))
		(format "%25s"
			(if (or (not mch) (<= len 25))
			    (substring from (max 0 (- len 25)))
			  (substring from
				     (setq lo (cond ((< (- mch 14) 0) 0)
						    ((< len (+ mch 11))
						     (- len 25))
						    (t (- mch 14))))
				     (min len (+ lo 25))))))))
          (if rmail-summary-line-count-flag
	      (save-excursion
		(save-restriction
		  (widen)
		  (let ((beg (rmail-msgbeg msgnum))
			(end (rmail-msgend msgnum))
			lines)
		    (save-excursion
		      (goto-char beg)
		      ;; Count only lines in the reformatted header,
		      ;; if we have reformatted it.
		      (search-forward "\n*** EOOH ***\n" end t)
		      (setq lines (count-lines (point) end)))
		    (format (cond
			     ((<= lines     9) "   [%d]")
			     ((<= lines    99) "  [%d]")
			     ((<= lines   999) " [%3d]")
			     (t		    "[%d]"))
			    lines))))
            " ")
	  " #"				;The # is part of the format.
	  (if (re-search-forward "^Subject:" nil t)
	      (progn (skip-chars-forward " \t")
		     (buffer-substring (point)
				       (progn (end-of-line)
					      (point))))
	    (re-search-forward "[\n][\n]+" nil t)
	    (buffer-substring (point) (progn (end-of-line) (point))))
	  "\n"))

;; WPS FIX made subject and quoting different, more like a rmail-resend
(defun rmail-forward (resend)
  "Forward the current message to another user.
With prefix argument, \"resend\" the message instead of forwarding it;
see the documentation of `rmail-resend'."
  (interactive "P")
  (if resend
      (call-interactively 'rmail-resend)
    (let ((forward-buffer (current-buffer))
	  (mail-signature "")
	  (msgnum rmail-current-message)
	  (subject (concat "Fwd: "
			   (or (mail-fetch-field "Subject") "")
			   )))
      ;; If only one window, use it for the mail buffer.
      ;; Otherwise, use another window for the mail buffer
      ;; so that the Rmail buffer remains visible
      ;; and sending the mail will get back to it.
      (if (funcall (if (and (not rmail-mail-new-frame) (one-window-p t))
		       (function mail)
		     (function rmail-start-mail))
		   nil nil subject nil nil nil
		   (list (list (function
				(lambda ()
				  (let ((msgnum
					 rmail-send-actions-rmail-msg-number))
				    (save-excursion
				      (set-buffer rmail-send-actions-rmail-buffer)
				      (if msgnum
					  (rmail-set-attribute
					   "forwarded" t msgnum)))))))))
	  ;; The mail buffer is now current.
	  (save-excursion
	    ;; We keep the rmail buffer and message number in these
	    ;; buffer-local vars in the sendmail buffer,
	    ;; so that rmail-only-expunge can relocate the message number.
	    (make-local-variable 'rmail-send-actions-rmail-buffer)
	    (make-local-variable 'rmail-send-actions-rmail-msg-number)
	    (setq rmail-send-actions-rmail-buffer forward-buffer)
	    (setq rmail-send-actions-rmail-msg-number msgnum)
	    ;; Insert after header separator--before signature if any.
	    (goto-char (point-min))
	    (search-forward-regexp
	     (concat "^" (regexp-quote mail-header-separator) "$"))
	    (forward-line 1)
	    ;; Quote lines with `- ' if they start with `-'.
	    (let ((beg (point)) end)
	      (setq end (point-marker))
	      (set-marker-insertion-type end t)
	      (insert-buffer-substring forward-buffer)
	      ;; Don't quote -'s any more
	      ;;(goto-char beg)
	      ;;(while (re-search-forward "^-" nil t)
	      ;;	(beginning-of-line)
	      ;;	(insert "- ")
	      ;;	(forward-line 1))
	      (goto-char end)
	      (skip-chars-backward "\n")
	      (if (< (point) end)
		  (forward-char 1))
	      (delete-region (point) end)
	      (set-marker end nil))
	    (push-mark))))))

;;; WPS FIX made recursive
(defun mail-strip-quoted-names (address)
  "Delete comments and quoted strings in an address list ADDRESS.
Also delete leading/trailing whitespace and replace FOO <BAR> with just BAR.
Return a modified address list."
  (let ((new-address (mail-strip-quoted-names-orig address)))
    (while (not (equal new-address address))
      (setq address new-address
	    new-address (mail-strip-quoted-names-orig new-address)))
    new-address))
;;(mail-strip-quoted-names "ad::sperber@ricks.enet.dec.com (TIMOTHY J. SPERBER \"Super Genius\" - HLO2-3/J5 \"Bats Cave\" DTN 225-4980  23-Jul-1996 0854)")

;;; WPS FIX added -orig
(defun mail-strip-quoted-names-orig (address)
  "Delete comments and quoted strings in an address list ADDRESS.
Also delete leading/trailing whitespace and replace FOO <BAR> with just BAR.
Return a modified address list."
  (if (null address)
      nil
    (if mail-use-rfc822
	(progn (require 'rfc822)
	       (mapconcat 'identity (rfc822-addresses address) ", "))
      (let (pos)
       (string-match "\\`[ \t\n]*" address)
       ;; strip surrounding whitespace
       (setq address (substring address
				(match-end 0)
				(string-match "[ \t\n]*\\'" address
					      (match-end 0))))

       ;; Detect nested comments.
       (if (string-match "[ \t]*(\\([^)\"\\]\\|\\\\.\\|\\\\\n\\)*(" address)
	   ;; Strip nested comments.
	   (save-excursion
	     (set-buffer (get-buffer-create " *temp*"))
	     (erase-buffer)
	     (insert address)
	     (set-syntax-table lisp-mode-syntax-table)
	     (goto-char 1)
	     (while (search-forward "(" nil t)
	       (forward-char -1)
	       (skip-chars-backward " \t")
	       (delete-region (point)
			      (save-excursion
				(condition-case ()
				    (forward-sexp 1)
				  (error (goto-char (point-max))))
				  (point))))
	     (setq address (buffer-string))
	     (erase-buffer))
	 ;; Strip non-nested comments an easier way.
	 (while (setq pos (string-match
			    ;; This doesn't hack rfc822 nested comments
			    ;;  `(xyzzy (foo) whinge)' properly.  Big deal.
			    "[ \t]*(\\([^)\"\\]\\|\\\\.\\|\\\\\n\\)*)"
			    address))
	   (setq address
		 (mail-string-delete address
				     pos (match-end 0)))))

       ;; strip `quoted' names (This is supposed to hack `"Foo Bar" <bar@host>')
       (setq pos 0)
       (while (setq pos (string-match
			  "[ \t]*\"\\([^\"\\]\\|\\\\.\\|\\\\\n\\)*\"[ \t\n]*"
			  address pos))
	 ;; If the next thing is "@", we have "foo bar"@host.  Leave it.
	 (if (and (> (length address) (match-end 0))
		  (= (aref address (match-end 0)) ?@))
	     (setq pos (match-end 0))
	   (setq address
		 (mail-string-delete address
				     pos (match-end 0)))))
       ;; Retain only part of address in <> delims, if there is such a thing.
       (while (setq pos (string-match "\\(,\\s-*\\|\\`\\)[^,]*<\\([^>,]*>\\)"
				      address))
	 (let ((junk-beg (match-end 1))
	       (junk-end (match-beginning 2))
	       (close (match-end 0)))
	   (setq address (mail-string-delete address (1- close) close))
	   (setq address (mail-string-delete address junk-beg junk-end))))
       address))))

;;;
;;; Mail in mode line
;;;

(defun display-time-mail-function ()
  (or (display-time-mail-function-file "~/Mail/.outlook")
      (display-time-mail-function-file "~/Mail/.spoolin")))

(defun display-time-mail-function-file (filename)
  (let ((attr (file-attributes (expand-file-name filename))))
    (and attr
	 (> (nth 7 attr) 120)   ; 100 bytes might just be RMAIL header
	 )))

(defvar display-time-mail-function nil)
(if (not display-time-mail-function)
    (setq display-time-mail-function `display-time-mail-function))
;(display-time-update)

;;;
;;; Mime decoding
;;;

(defun rmail-mime-decrypt-region ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward "\*\*\* EOOH")
    (re-search-forward "^$")
    (forward-line 1)
    (narrow-to-region (point) (point-max))

    (goto-char (point-min))
    (cond ((or t (search-forward "content-transfer-encoding: quoted-printable" nil t))
	   (global-replace-regexp "=\n" "")
	   (global-replace-regexp "=09" "\t")
	   (global-replace-regexp "=0a" "\n")
	   (global-replace-regexp "=0d" "")
	   (global-replace-regexp "=20" " ")
	   (global-replace-regexp "=2e" ".")
	   (global-replace-regexp "=a0" " ")
	   (global-replace-regexp "=3d" "=")  ;; Must be last
	   ))
    (goto-char (point-min))
    (cond ((or t (re-search-forward "\\(<html>\\|<title>\\|<p>\\)" nil t))
	   (global-replace-regexp "<br>" "\n")
	   (global-replace-regexp "<[^>]+>" "")
	   (global-replace-regexp "\&gt;" ">")
	   (global-replace-regexp "\&lt;" "<")
	   (global-replace-regexp "\&amp;" "&")
	   (global-replace-regexp "\&nbsp;" " ")
	   (global-replace-regexp "\&quot;" "\"")
	   ;;
	   (global-replace-regexp "\n[ \t]*\n[ \t]*\n+" "\n\n")
	   ))
    (goto-char (point-min))
    (cond ((search-forward "Content-Type: message" nil t)
	   (global-delete-matching-lines rmail-ignored-headers)
	   ))
    (goto-char (point-min))
    (cond ((or (search-forward "This is a multi-part message in MIME format." nil t)
	       (search-forward "content-transfer-encoding" nil t))
	   (global-delete-matching-lines "^This is a multi-part message in MIME format.\\s *$")
	   (global-delete-matching-lines "^------")
	   (global-delete-matching-lines "^content-type: text/plain")
	   (global-delete-matching-lines "^content-type: message")
	   (global-delete-matching-lines "^content-disposition: inline")
	   (global-delete-matching-lines "^content-transfer-encoding: 7bit")
	   (global-delete-matching-lines "^content-transfer-encoding: quoted-printable")
	   ))))

(defun rmail-mime-decrypt ()
  "De-MIME a rmail message."
  (interactive)
  (widen)
  (narrow-to-region (rmail-msgbeg rmail-current-message)
		    (rmail-msgend rmail-current-message))
  (let ((buffer-read-only nil))
    (rmail-mime-decrypt-region)))
;  (rmail-show-message rmail-current-vmessage))

;;;
;;; Mime outputting
;;;

(defun rmail-mime-output (out-filename)
  "Save a MIME component."
  (interactive "FMime output filename: ")
  (if (string-match "/Mail/" out-filename)
      (error "Can't put mime output into Mail folder, perhaps you wanted rmail output instead?"))
  (widen)
  (narrow-to-region (rmail-msgbeg rmail-current-message)
		    (rmail-msgend rmail-current-message))
  (let ((tmp-filename "/tmp/wsnyder_mime")
	(case-fold-search t)
	end-pt)
    (save-excursion
      (search-forward "content-transfer-encoding")
      (save-excursion (setq end-pt (if (search-forward "content-transfer-encoding" nil t)
				       (point)
				     (point-max))))
      (forward-line -1)
      (write-region (point) end-pt tmp-filename)
      (shell-command (concat "b64 < " tmp-filename " > " out-filename) nil nil)
      )
    (goto-char end-pt)
    (forward-line -2)))

;;;
;;; Web brouser launching
;;;

(defun rmail-web-output (url)
  "Save a web url."
  (interactive (list (read-from-minibuffer
		      (concat "URL: ")
		      (if (looking-at "\\s *\\([^ \t\r\n]+\\)")
			  (match-string 1)))))
  (save-excursion
    (shell-command (concat "browser '" url "'"))))

;;;
;;; Outlook
;;;

(defun rmail-outlook-date ()
  "Edit date for outlook forwarded messages"
  (interactive)
  (rmail-edit-current-message)
  (goto-char (point-min))
  (global-replace-regexp "^> " "")
  (delete-region (point)
		 (save-excursion
		   (re-search-forward "^ *-----Original Message")
		   (forward-line 1)
		   (point)))
  (re-search-forward "^Sent")
  (forward-line 0)
  (delete-region (point) (+ (point) 4))  (insert "Date")
  (rmail-cease-edit))

(defun rmail-busy ()
  (interactive)
  (insert "\n"
	  "Sorry I don't have time to respond to these types of email.\n"
	  "Please remove me from any mailing lists.\n"
	  "\n"
	  "Thanks.\n"))

;;;
;;; Have quit exit everybody
;;;

(defun rmail-expunge-and-save-orig ()
  "Expunge and save RMAIL file."
  (interactive)
  (rmail-expunge)
  (save-buffer)
  (if (rmail-summary-exists)
      (rmail-select-summary (set-buffer-modified-p nil))))

(defun rmail-expunge-and-save ()
  "Expunge and save ALL RMAIL file."
  (save-excursion
    (mapcar (function (lambda (buffer)
			(and
			 (buffer-name buffer)	; Makes sure that buffer exists
			 (set-buffer buffer)
			 (eq major-mode 'rmail-mode)
			 (progn (rmail-expunge-and-save-orig)
				(kill-buffer buffer))
			 )))
	    (buffer-list))))


(defun rmail-quit-kill ()
  "Quit out of RMAIL.  Delete all buffers too."
  (interactive)
  (save-excursion
    (rmail-quit)
    (mapcar (function (lambda (buffer)
			(unwind-protect
			    (and
			     (set-buffer buffer)
			     (eq major-mode 'rmail-mode)
			     (kill-buffer buffer)))))
	    (buffer-list))))

(defun rmail-quit-expunge ()
  "Quit out of RMAIL.  Save buffers and expunge, exc outlook."
  (interactive)
  (save-excursion
    ;; Save no-expunge outlook, rest expunge
    (mapcar (function (lambda (buffer)
			(unwind-protect
			  (set-buffer buffer)
			  (when (eq major-mode 'rmail-mode)
			    (cond ((string-match ".outlook" (buffer-file-name))
				   (let ((rmail-confirm-expunge (function (lambda (msg) nil))))
				     (rmail-expunge-and-save-orig)
				     (kill-buffer buffer)))
				  (t
				   (let ((rmail-confirm-expunge (function (lambda (msg) t))))
				     (rmail-expunge-and-save-orig)
				     (kill-buffer buffer))))
			    ))))
	    (buffer-list))
    ;; Normal save
    (rmail-quit)
    (display-time-update)
    ))

;;;
;;; Timer
;;;

(defvar rmail-timer-interval
  (* 30 60 )
  "Seconds of idle time before flushing rmail.")

(defvar rmail-timer-running nil "Is the rmail timer running?")
(defun rmail-timer (&optional startup)
  "Periodic clearing timer for flushing mail."
  (interactive)
  (cond ((not startup)
	 (message "Timer saving rmail buffers")
	 (rmail-expunge-and-save)))
  (cond ((not rmail-timer-running)
	 (run-with-idle-timer rmail-timer-interval t 'rmail-timer)
	 (setq rmail-timer-running t))))

(rmail-timer t)
