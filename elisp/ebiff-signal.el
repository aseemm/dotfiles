;; Mail detection signalling
;; wsnyder@wsnyder.org

(provide 'ebiff-signal)

;;;;
;;;; Variables
;;;;

(defvar ebiff-pid-filename (expand-file-name "~/Mail/.pbiffpid"))
(defvar ebiff-signal-process nil)

;;;;
;;;; Signalling startup
;;;;


(defun ebiff-signal-daemon ()
  (if ebiff-signal-process
      (delete-process ebiff-signal-process))
  (setq ebiff-signal-process
	(start-process "ebiff-signaler" " *ebiff-signaler*" "sleep" "2000000000"))
  (set-process-sentinel ebiff-signal-process 'ebiff-signal-sentinel)
  (ebiff-write-pid (process-id ebiff-signal-process)))

(defun ebiff-signal-sentinel (process event)
  (message (format "Ebiff Signaled, event %s" process event))
  (ebiff-signal-daemon)	;; Make new daemon, old is dead
  (ebiff-timer-force)
  )

(defun ebiff-write-pid (pid)
  (save-excursion
    (find-file ebiff-pid-filename)
    (erase-buffer)
    (insert (number-to-string pid) "\n")
    (save-buffer)
    (kill-buffer nil)))


;;;;
;;;; Signalling sending
;;;;

(defun ebiff-signal-reread ()
  "Tell ebiff that messages have been read/sent and it should reread."
  (interactive)
  (save-excursion
    (display-time-update)
    (cond ((file-exists-p ebiff-pid-filename)
	   (find-file ebiff-pid-filename)
	   (goto-char (point-min))
	   (cond ((looking-at "^[0-9]+$")
		  (let ((pid (string-to-number (match-string 0))))
		    ;(message "Ebiff signaled pid %s" pid)
		    (signal-process pid `SIGUSR1)
		    )))
	   (kill-buffer nil)
	   ))))

;;;;
;;;; Hook to signal on mail reading
;;;;

(add-hook 'rmail-get-new-mail-hook 'ebiff-signal-reread)
(add-hook 'rmail-quit-hook 'ebiff-signal-reread)

