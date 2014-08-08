;;; $Id$
;;; These are general mail features, see also rmail-add for rmail specifics

(provide 'mail-add)

;;;;
;;;; Defines
;;;;


;;;;
;;;; Fetching conversions
;;;;

;(defun mail-convert-new-mail ()
; ... see CVS history
;(add-hook 'rmail-get-new-mail-hook 'mail-convert-new-mail)


;;;;
;;;; Bouncing
;;;;

(defun mail-bounce-from (body)
  (if (string-match "^from: \\(.*\\)" body)
      (match-string 1 body)
    nil))

(defun mail-bounce-host (body)
  (cond ((string-match "veripool.com" body) "veripool.com")
	((string-match "world.std.com" body) "world.std.com")
	((string-match "wsnyder.org" body) "wsnyder.org")
	((string-match "iname.com" body) "iname.com")
	((string-match "ultranet.com" body) "ultranet.com")	;; POP server must be last
	"wsnyder.org"))

(defun mail-bounce-reply-to (body from)
  (if (string-match "^reply-to: \\(.*\\)" body)
      (match-string 1 body)
    from))

(defun mail-bounce ()
  (interactive)
  (save-excursion
    (rmail-toggle-header 0)
    (let* ((body (buffer-substring (point-min) (point-max)))
	   (from (mail-bounce-from body))
	   (reply-to (mail-bounce-reply-to body from))
	   ;(reply-to "wsnyder")   ;; from or wsnyder for debug
	   (host (mail-bounce-host body))
	   (to (concat "wsnyder@" host)))
      (set-buffer (get-buffer-create "*bounce*"))
      (erase-buffer)
      (insert "From: Mail Delivery Subsystem <MAILER-DAEMON>\n")
    (insert "To: " reply-to "\n")
    (insert "MIME-Version: 1.0\n")
    (insert "Content-Type: multipart/report; report-type=delivery-status;\n")
    (insert "	boundary=\"JAA28333.924528010/" host "\"\n")
    (insert "Subject: Returned mail: User unknown\n")
    (insert "Auto-Submitted: auto-generated (failure)\n")
    (insert "\n")
    (insert "This is a MIME-encapsulated message\n")
    (insert "\n")
    (insert "--JAA28333.924528010/" host "\n")
    (insert "\n")
    (insert "The original message was received at " (current-time-string) "\n")
    (insert "from " from "\n")
    (insert "\n")
    (insert "   ----- The following addresses had permanent fatal errors -----\n")
    (insert to "\n")
    (insert "    (expanded from: " to ")\n")
    (insert "\n")
    (insert "   ----- Transcript of session follows -----\n")
    (insert "... while talking to mailhub:\n")
    (insert ">>> RCPT To:<" to ">\n")
    (insert "<<< 550 <" to ">... User unknown\n")
    (insert "550 " to "... User unknown\n")
    (insert "\n")
    (insert "--JAA28333.924528010/" host "\n")
    (insert "Content-Type: message/delivery-status\n")
    (insert "\n")
    (insert "Reporting-MTA: dns; " host "\n")
    (insert "Arrival-Date: " (current-time-string) " (EDT)\n")
    (insert "\n")
    (insert "Final-Recipient: rfc822; " to "\n")
    (insert "X-Actual-Recipient: rfc822; " to "\n")
    (insert "Action: failed\n")
    (insert "Status: 5.1.1\n")
    (insert "Remote-MTA: dns; mailhub\n")
    (insert "Diagnostic-Code: smtp; 550 <" to ">... User unknown\n")
    (insert "Last-Attempt-Date: " (current-time-string) " (EDT)\n")
    (insert "\n")
    (insert "--JAA28333.924528010/" host "\n")
    (insert "Content-Type: message/rfc822\n")
    (insert "\n")
    (insert body)
    (insert "\n")
    (insert "--JAA28333.924528010/" host "\n")
    (shell-command-on-region
     (point-min) (point-max)
     (concat "/usr/lib/sendmail -F 'Mail Delivery Subsystem' -f MAILER-DAEMON '" reply-to "'")
     ))
    )
  (rmail-delete-forward)
  )

(defun mail-junk ()
  (interactive)
  (rmail-output-to-rmail-file (expand-file-name "~/Mail/junk_out"))
  )
