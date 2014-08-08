;;; mod-funcs.el --- Misc Support/User Functions
;;; Wilson Snyder, wsnyder@wsnyder.org
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

(provide 'mod-funcs)

;; in new-lisp/0otherstuff.el
;; Simple loop - no longer used.  Uses old-style backquotes, so deprecated.
;; (for-loop x 0 25 (insert "Hi!\n"))
;;(defmacro for-loop (var init final &rest body)
;;  "Execute a for loop.
;;Uses VAR starting at INIT counting up to FINAL, executing BODY each time."
;;  (` (let (( (, var) (1- (, init)) ))
;;       (while (>= (, final) (setq (, var) (1+ (, var))))
;;         (,@ body)))))

;; Speed saver
(defvar auto-save-size-limit nil "Size of file past which auto save is disabled, nil=off.")

(defun auto-save-size-check ()
  "Check the auto-save-size-limit"
  (when (and auto-save-size-limit
	     (> (- (point-max) (point-min)) auto-save-size-limit))
    (auto-save-mode nil)
    (message "Auto-save off (in this buffer) since file is large")))
(add-hook 'find-file-hooks 'auto-save-size-check)

;; in new-lisp/0otherstuff.el
;;
;; Line wrapping support
(defun toggle-truncate-lines (arg)
  "Toggle truncation of long screen lines.
With arg, turn truncation on iff arg is positive."
  (interactive "P")
  (setq truncate-lines
	(if (null arg) (not truncate-lines)
	  (> (prefix-numeric-value arg) 0)))
  (message "line truncation %s."
	   (if truncate-lines "ON" "OFF"))
  (redraw-display))

;; in new-lisp/0otherstuff.el
;;
;;
;; Exact (case-fold-search)
(defun toggle-case-fold-search (arg)
  "Toggle case folding on searches.
With arg, turn folding on iff arg is positive."
  (interactive "P")
  (setq case-fold-search 
	(if (null arg) (not case-fold-search)
	  (> (prefix-numeric-value arg) 0)))
  (message "Searches %s case."
	   (if case-fold-search "fold" "use exact")))

;; in new-lisp/0otherstuff.el
;;
;;
;; Debug on error mode
(defun toggle-debug-on-error (arg)
  "Toggle debugging on error.
With arg, turn folding on iff arg is positive."
  (interactive "P")
  (setq debug-on-error
	(if (null arg) (not debug-on-error)
	  (> (prefix-numeric-value arg) 0)))
  (message "%s debug on errors."
	   (if debug-on-error  "Will" "Will not")))

;;
;;
(defun eval-defun-comment (arg)
  "Evaluate defun that is commented on this line
Print value in minibuffer.
With argument, insert value in current buffer after the defun."
  (interactive "P")
  (let ((begin (point)))
    (save-excursion
      (end-of-defun)
      (let ((end (point)))
	(goto-char begin)
	(beginning-of-line)
	(while (looking-at ";")
	  (forward-char 1))
	(eval-region (point) end
		     (if arg (current-buffer) t))))))


(defun parallel-resistors (a &rest b)
  "Calculate the resistance of any number of parallel resistors."
  (let ((tot (/ 1.0 a)))
    (while b (setq tot (+ tot (/ 1.0 (car b)))
		   b (cdr b)))
    (/ 1.0 tot)))

;; 1/23/98 WPS
(defun visit-tags-table-if-exists (filename)
  "Visit tags table if exists and not already visited"
  (if (and (file-exists-p filename)
	   (not (or (member filename tags-table-list)
		    (equal filename tags-file-name))))
      (visit-tags-table filename)))

;; 10-20-94 WPS
;; Emacs lisp function for visiting common tag files.
(defun visit-emacs-lisp-tags-table ()
  "Visit tags tables for the load-path variables and source directories.
You may want to add this as a hook to emacs-lisp-mode:

 (add-hook 'emacs-lisp-mode-hook 'visit-emacs-lisp-tags-table)
 "
  (mapcar '(lambda (path) 
	     (visit-tags-table-if-exists
	      (expand-file-name (concat path "/TAGS")))
	     (visit-tags-table-if-exists
	      (expand-file-name (concat path "/../src/TAGS"))))
	  load-path))

(defun save-and-compile ()
  "Expand auto declarations if verilog, save the files, and compile the
current buffer."
  (interactive)
  (if (equal mode-name "Verilog")
      (verilog-create-autos))
  (save-buffer)
  ;;(save-some-buffers nil nil)
  (compile compile-command))

;;;;
;;;;
;;;;
;;;;

;; In new-lisp/sort.el
;;;###autoload
(defun sort-columns-untabify (reverse &optional beg end)
  "Sort lines in region alphabetically by a certain range of columns.
For the purpose of this command, the region includes
the entire line that point is in and the entire line the mark is in.
The column positions of point and mark bound the range of columns to sort on.
A prefix argument means sort into reverse order.

Tabs will be removed, then added back.  Note that the tabs
that are reinserted may not be in the same place as the
tabs in the original region."
  (interactive "P\nr")
  (save-excursion
    (let (linebeg colbeg lineend colend)
      (goto-char (min beg end))
      (setq linebeg (count-lines 1 (point))
	    colbeg  (current-column))
      (goto-char (max beg end))
      (setq lineend (count-lines 1 (point))
	    colend  (current-column))
      ;; detabify whole line
      (untabify (progn (goto-line linebeg)
		       (point))
		(progn (goto-line (1+ lineend))
		       (point)))
      ;; adjust points after tabify
      (sort-columns reverse
		    (progn (goto-line linebeg)
			   (move-to-column colbeg)
			   (point))
		    (progn (goto-line lineend)
			   (move-to-column colend)
			   (point)))
      ;; retabify whole line
      (tabify (progn (goto-line linebeg)
		     (point))
	      (progn (goto-line (1+ lineend))
		     (point)))
      )))

;;;
;;; Uniq 10/20/93 WPS
;;;

;; In new-lisp/replace.el
(defun uniq-lines ()
  "Remove duplicate lines, al la Unix UNIQ."
  (interactive)
  (save-excursion
    (let ((last-line nil)
	  (this-line nil))
      (beginning-of-line)
      (while (not (eobp))
	(setq this-line (buffer-substring (prog1 (point) (forward-line 1))
					  (point)))
	(if (equal this-line last-line)
	    (progn 
	      (beginning-of-line 0)
	      (delete-region (point) (1+ (point-at-eol))))
	  (setq last-line this-line))))))

;;;
;;; Global searches, 8/4/94 WPS
;;;

;; in new-lisp/0otherstuff.el
(defun global-replace-regexp (regexp to-string &optional thrown-away)
  "Globally replace a regular expression with a new expression.
See replace-regexp."
  (interactive (query-replace-read-args "Global replace regexp" t))
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (replace-match to-string nil nil))
  )

;; in new-lisp/0otherstuff.el
(defalias 'global-delete-matching-lines 'global-flush-lines)
(defun global-flush-lines (regexp)
  "Globally delete lines containing matches for REGEXP.
If a match is split across lines, all the lines it lies in are deleted.
Applies to lines after point."
  (interactive (list (read-from-minibuffer
		      "Globally flush lines (containing match for regexp): "
		      nil nil nil 'regexp-history)))
  (goto-char (point-min))
  (flush-lines regexp))

;; in new-lisp/0otherstuff.el
(defun delete-logic-io-lines ()
  "Delete lines that are verilog inputs.
This is useful for tracing logic via grep."
  (interactive)
  (flush-lines "\\<input\\>")
  (flush-lines "/\\*AUTOSENSE\\*/")
  (flush-lines "/\\*AS\\*/")
  (flush-lines "\\<\\(wire\\|reg\\|integer\\)\\>[^=\n]*;\\s *\\($\\|//\\)")
  (flush-lines "\\.\\s *\\([a-zA-Z0-9_]+\\)\\s *(\\s *\\1\\s *[[)]"))

(defun flip-logic-inputs ()
  "Put signal names first in I/O lists for easy sorting"
  (interactive)
  (global-replace-regexp "[ \t]*//.*$" "")
  (global-replace-regexp "^[ 	]*\\(input\\|inout\\|output\\)[ 	]*\\([][0-9:]*\\)[ 	]+\\([^ 	:;]+\\)[ 	;]*$" "\\3			\\1\\2")
  (global-replace-regexp "^[ 	]*\\(sc_in\\|sc_out\\)\\(.*<.*<.*>.*>\\)[ 	]+\\([^ 	:;]+\\)[ 	;]*$" "\\3			\\1\\2")
  (global-replace-regexp "^[ 	]*\\(sc_in\\|sc_out\\)\\(.*<.*>\\)[ 	]+\\([^ 	:;]+\\)[ 	;]*$" "\\3			\\1\\2"))

(defun combine-same-prefix-lines()
  "If there are two lines with same prefix, merge onto one line"
  (interactive)
  (global-replace-regexp "^\\([^ \t\n]+\\)\\([^\n]+\\)\n\\1[ \t]\\([^\n]+\\)\n" "\\1\\2\t\\3\n"))

;;;
;;; Exiting
;;;

(defvar kill-emacs-message-limit 8 "Number of buffers that if exist will override exiting.")
(defun kill-emacs-message ()
  "If more then kill-emacs-message-limit buffers exist,
then print a warning instead of exiting."
  (interactive)
  (cond ((> (let ((len 0)
		  (buf-list (buffer-list)))
	      (while buf-list
		(or (string-match "^\\([ *]\\|TAGS\\)" (buffer-name (car buf-list)))
		    (save-excursion (set-buffer (car buf-list)) (eq major-mode `dired-mode))
		    (setq len (1+ len)))
		(setq buf-list (cdr buf-list)))
	      len)
	    kill-emacs-message-limit)
	 (message "Type M-x save-buffers-kill-emacs to exit emacs."))
	(t
	 (save-buffers-kill-emacs))))
;; Put this in to disable the C-x\C-c combo:
;;(global-set-key "\C-x\C-c" 'kill-emacs-message)


;;;
;;; Code Profiler
;;;

(defun profile-stamp (descr) "Timestamp checkpoint."
  (setq profile-timer (cons (list descr (current-time)) profile-timer)))

(defun profile-start () "Start profile timer."
  (setq profile-timer nil)
  (profile-stamp "Start"))

(defun profile-result () "Print profile results."
  (interactive)
  (with-output-to-temp-buffer "*profile*"
    (save-excursion
      (set-buffer "*profile*")
      (profile-stamp "Print Results")
      (let ((tim (reverse profile-timer)))
	(cond (tim
	       (let ((sec (nth 1 (nth 1 (car tim))))
		     (mic (nth 2 (nth 1 (car tim))))
		     (name (car (car tim))))
		 (while tim
		   (let ((dsec (nth 1 (nth 1 (car tim))))
			 (dmic (nth 2 (nth 1 (car tim))))
			 (dname (nth 0 (car tim)))
			 difmic difsec)
		     (setq difmic (- dmic mic)
			   difsec (- dsec sec))
		     (if (< difmic 0) (setq difmic (+ 1000000 difmic) difsec (1- difsec)))
		     (insert name "\t" (int-to-string difsec) "." (int-to-string (/ difmic 1000)) "\n")
		     (setq tim (cdr tim)
			   sec dsec
			   mic dmic
			   name dname))))))))))

;;;
;;; RCS Stuff, WPS 3/31/95
;;;

(autoload 'vc-checkout "vc" "" t)
(defun co ()
  "Retrieve a copy of the latest version of the given file.  Defaults to buffer-file-name."
  (interactive)
  (vc-revert-buffer))

(autoload 'vc-checkin "vc" "" t)
(defun ci (&optional file rev comment)
  "Check in the file specified by FILE.
The optional argument REV may be a string specifying the new version level
(if nil increment the current level).  The file is either retained with write
permissions zeroed, or deleted (according to the value of `vc-keep-workfiles').
COMMENT is a comment string; if omitted, a buffer is
popped up to accept a comment.

File defaults to buffer-file-name."
  (interactive)
  (if (not file) (setq file (buffer-file-name)))
  (vc-checkin file rev comment))

;;;
;;; Filenames, WPS 7/12/95
;;;

(defun unique-filename (template ext)
  "Return a unique filename in the form TEMPLATE_random.EXT."
  (let ((nogood t) (i 0) tempname)
    (while nogood
      (setq tempname (concat template "_" (user-real-login-name)
			     "_" (number-to-string (emacs-pid)) "_"
			     (number-to-string i) ext ))
      (setq nogood (file-exists-p tempname))
      (setq i (1+ i)))
    tempname))


;;;
;;; Displays and Frames, 7/7/95  WPS
;;;

(defun disp-n (n) "Set new frames to display on screen number n."
  (let ((disp (cdr (assq 'display (frame-parameters)))))
    (setq disp (if (string-match "^\\(.*:0\\.\\)[0-9]" disp)
		   (concat (match-string 1 disp) (int-to-string n))
		 disp))
    ;; Apply it to default-frame-alist.
    (let ((parameter (assq 'display default-frame-alist)))
      (if (consp parameter)
	  (setcdr parameter disp)
	(setq default-frame-alist
	      (cons (cons 'display disp)
		    default-frame-alist))))))

(defun disp0 () "Set new frames to display on screen 0.
Then use \\[make-frame] to pop up a new frame on that display."
  (interactive) (disp-n 0))
(defun disp1 () "Set new frames to display on screen 1.
Then use \\[make-frame] to pop up a new frame on that display."
  (interactive) (disp-n 1))

;; 1/26/94 WPS
(defun set-frame-font (font)
  "Set the frame to use FONT.  Interactively, prompt for a specific font.
If a number is specified instead of a font, the font will be changed to
that point size."
  (interactive "sFont name (or 8 9 10 13 14 15 20): ")
  (if (> (string-to-int font) 0)
      (setq font (concat "-misc-fixed-medium-r-normal--" font "-*-*-*-*-*-iso8859-1")))
  (modify-frame-parameters (selected-frame) (list (cons 'font font))))

;; 1/26/94 WPS
(defun set-frame-name (name)
  "Set the frame to use NAME.  Interactively, prompt for a specific name."
  (interactive "sFrame name: ")
  (modify-frame-parameters (selected-frame) (list (cons 'name name))))

;;;
;;; All-buffer functions, 5/14/96 WPS
;;;

(defvar allbuf-grep-history nil)

(defun allbuf-grep (command-args)
  "Search all buffers for a given string

This command uses a special history list for its arguments, so you can
easily repeat a grep command."
  (interactive
   (list (read-from-minibuffer "Grep buffers for: "
			       "" nil nil 'allbuf-grep-history)))
  (require 'compile)
  (let ((buf (allbuf-internal "No more allbuf-grep hits!" "allbuf-grep" nil grep-regexp-alist)))
    (save-excursion
      (set-buffer buf)
      (insert "Searching all buffers...\n\n")
      (mapcar (function (lambda (elt)
			  (set-buffer elt)
			  (if buffer-file-name
			      (save-excursion
				(goto-char (point-min))
				(while (re-search-forward command-args nil t)
				  (let ((finfo (concat buffer-file-name
						       ":" (number-to-string (count-lines (point-min) (point))) ":"))
					(ln (buffer-substring (save-excursion (beginning-of-line) (point)) (progn (end-of-line) (point)))))
				    (forward-line 1)
				    (save-excursion
				      (set-buffer buf)
				      (insert finfo ln "\n"))))))))
	      (buffer-list))
      )))

;; Similar to compile-internal
(defun allbuf-internal (&optional error-message name-of-mode parser regexp-alist)

  "Set up a buffer for dumping information.  First argument NAME-OF-MODE is
the name to display as the major mode in the compilation buffer.
Second arg PARSER is the error parser function (nil means the default).  Third
arg REGEXP-ALIST is the error message regexp alist to use (nil means the
default).

Returns the compilation buffer created."
  (let (outbuf
	(parser (or parser compilation-parse-errors-function)))
    (save-excursion
      (setq outbuf (get-buffer-create (concat "*" name-of-mode "*")))
      (set-buffer outbuf)
      ;; In case the compilation buffer is current, make sure we get the global
      ;; values of compilation-error-regexp-alist, etc.
      (kill-all-local-variables)
      ;;
      (setq buffer-read-only nil)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (buffer-enable-undo (current-buffer))
      ;;
      (set-buffer-modified-p nil)
      ;; Pop up the compilation buffer.
      (setq outwin (display-buffer outbuf))
      (save-excursion
	(set-buffer outbuf)
	(compilation-mode)
	;; (setq buffer-read-only t)  ;;; Non-ergonomic.
	(set (make-local-variable 'compilation-parse-errors-function) parser)
	(set (make-local-variable 'compilation-error-message) error-message)
	(set (make-local-variable 'compilation-error-regexp-alist) regexp-alist)
	;;(setq default-directory thisdir
	;;      compilation-directory-stack (list default-directory))
	(set-window-start outwin (point-min))
	(setq mode-name name-of-mode)
	(or (eq outwin (selected-window))
	    (set-window-point outwin (point-min)))
	(compilation-set-window-height outwin)

	;; Make it so the next C-x ` will use this buffer.
	(setq compilation-last-buffer outbuf)
	outbuf))))

;;;
;;; Printing additions
;;;

(autoload (quote print-region-1) "lpr" "Internal autoload." nil nil)
(defun lpr-buffer-choose-ps ()
  "Print buffer contents as with Unix command `lpr'.
If a postscript string is found, use 'lpr-ps-switches'.
Else `lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (cond ((search-forward "PS-Adobe" nil t)
	   ;; Is postscript
	   (forward-line 0)
	   (print-region-1 (point) (point-max) lpr-ps-switches nil))
	  (t ;; Normal
	   (print-region-1 (point-min) (point-max) lpr-switches nil)))
    ))

;;;
;;; Grep additions
;;;

;; In new-lisp/compile.el
(require 'compile)
(autoload (quote find-tag-default) "etags" "")
(if (>= emacs-major-version 22)
  (defalias 'grep-defaulted 'grep)
 (defun grep-defaulted (command-args)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a grep command.

Passing a prefix-argument defaults the argument based upon the current
tag the cursor is over."
  (interactive
   (let (grep-default)
     (cond ((and current-prefix-arg
		 grep-history)
	    (let* ((tag-default (funcall (or find-tag-default-function
					     (get major-mode 'find-tag-default-function)
					     'find-tag-default))))
	      (if (and tag-default (string-match "`.*$" tag-default))
		  (setq tag-default (replace-match "" t t tag-default)))
	      ;; Default to last grep command
	      (setq grep-default (car grep-history))
	      ;; Replace the thing matching for with that around cursor
	      (if (string-match "\\s +-e\\s +\\([^ ]+\\)+" grep-default)
		  (setq grep-default (replace-match tag-default t t grep-default 1))))))
     (list (read-from-minibuffer "Run grep (like this): "
				 (or grep-default grep-command) nil nil 'grep-history))))
  ;; Setting process-setup-function makes exit-message-function work
  ;; even when async processes aren't supported.
  (let* ((compilation-process-setup-function 'grep-process-setup)
	 (buf (compile-internal (if grep-null-device
				    (concat command-args " " grep-null-device)
				  command-args)
				"No more grep hits" "grep"
				;; Give it a simpler regexp to match.
				nil grep-regexp-alist))))))

(if (eq emacs-major-version 23)
  (defun grep-default-command ()
  "Compute the default grep command for C-u M-x grep to offer."
  (let ((tag-default (shell-quote-argument (grep-tag-default)))
	;; This a regexp to match single shell arguments.
	;; Could someone please add comments explaining it?
	(sh-arg-re "\\(\\(?:\"\\(?:[^\"]\\|\\\\\"\\)+\"\\|'[^']+'\\|[^\"' \t\n]\\)+\\)")
	(grep-default (or (car grep-history) grep-command)))
    ;; In the default command, find the arg that specifies the pattern.
    (when (or (string-match
	       (concat "[^ ]+\\s +\\(?:-[^ ]+\\s +\\)*"
		       sh-arg-re "\\(\\s +\\(\\S +\\)\\)?")
	       grep-default)
	      ;; If the string is not yet complete.
	      (string-match "\\(\\)\\'" grep-default))
      ;;--- HERE we removed file defaulting
      ;; Now replace the pattern with the default tag.
      (replace-match tag-default t t grep-default 1)))))

(defun occur-defaulted (regexp &optional nlines)
  "Show all lines in the current buffer containing a match for REGEXP.

If a match spreads across multiple lines, all those lines are shown.

Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.
NLINES defaults to `list-matching-lines-default-context-lines'.
Interactively it is the prefix arg.

The lines are shown in a buffer named `*Occur*'.
It serves as a menu to find any of the occurrences in this buffer.
\\[describe-mode] in that buffer will explain how."
  (interactive
   (let (occur-default)
     (cond (current-prefix-arg
	    (setq occur-default (funcall (or find-tag-default-function
					     (get major-mode 'find-tag-default-function)
					     'find-tag-default))))
	   (regexp-history
	    (setq occur-default (car regexp-history))))
     (list (read-from-minibuffer "List lines matching regexp: "
				 occur-default nil nil 'regexp-history))
     ))
  (occur regexp nlines))

;; 1/22/1998 wps
;; 1/27/2011 wps disabled as no longer used, uses old-style backquotes
;;(defmacro wean-key (old-command)
;;  "Raise notice that a old command binding has changed.
;;This special macro returns a command that can be bound to a key."
;;  (` (lambda () (interactive) (wean-command (, old-command)))))
;;
;;(defun wean-command (old-command)
;;  "Raise notice that a old command binding has changed."
;;  (interactive)
;;  (let (debug-on-error)
;;    (error (format "Error: Key for %S is now %s" old-command
;;		   (substitute-command-keys (concat "\\[" (format "%S" old-command) "]"))))))

;; 12/13/99 wps
(defun elsy-lock ()
  (interactive)
  (shell-command "get_merge_lock elsy; release_merge_lock elsy"))

;; 3/30/2000 wps
(defun del ()
  (interactive)
  (define-key global-map "\C-h" 'backward-delete-char)
  (setq search-delete-char 8)
  (define-key global-map "\177" 'delete-char)
  )

;; 2004 rv
(defun tail-file (file)
  "Tail a given file inside and emacs buffer"
  (interactive "fFile to tail: ")
  (let ((cmd (concat "tail -f " file "&"))
	(buf (concat "*tail -f " (file-name-nondirectory file) "*")))
    (shell-command cmd buf)))


;; 2007-01 wps
(defun yank-kill-ring ()
  "Yank the entire kill ring"
  (interactive)
  (let* ((kills kill-ring) kill)
    (while kills
      (setq kill (car kills)
	    kills (cdr kills))
      (unless (= 0 (current-column)) (insert "\n"))
      (insert "-----\n")
      (insert kill)))
  (unless (= 0 (current-column)) (insert "\n"))
  (insert "-----\n"))

;; 2009/01/26 WPS
(defun c-no-electric ()
  "Kill electric keys in c++-mode, such as for bison editing"
  (interactive)
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key "/" 'self-insert-command)
  (local-set-key "\t" 'self-insert-command)
  (local-set-key "<" 'self-insert-command)
  (local-set-key ">" 'self-insert-command)
  (local-set-key "{" 'self-insert-command)
  (local-set-key "}" 'self-insert-command)
  (local-set-key "(" 'self-insert-command)
  (local-set-key ")" 'self-insert-command)
  (local-set-key "," 'self-insert-command)
  (local-set-key ";" 'self-insert-command))

;; 2009/01/26 WPS
(defun tilt (start end)
  "Rotate a text region between BEGIN and END 90 degrees.
See also yank-tilted-rectangle."
  (interactive "r")
  (shell-command-on-region start end "tilt" t t))

;; 2010/04/16 WPS - Checkdoc expects this when loading verilog-mode.el into emacs-lisp-mode
(unless (fboundp 'booleanp)
  (defun booleanp (value)
    "Return t if VALUE is boolean.
This implements GNU Emacs 22.1's `booleanp' function in earlier Emacs.
This function may be removed when Emacs 21 is no longer supported."
    (or (equal value t) (equal value nil))))
