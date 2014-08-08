; $Id$
;; Mode for editing book list
;; Wilson Snyder, BINKLY::SNYDER  9/3/92
(provide 'books)

;; Tab definitions
;;"Robbe-Grillet, Alain                                        The Secret Room                                                         94.07    4 JOINT   CLASSIC  6  MRO Literature Text                               1962 Description"
;;"Aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa Ttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt Ddddd Pppp Mmmmmmt Sssssss Rrr Lll jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj Dddddddddddddddd"
(defconst books-tab-author	0)
(defconst books-tab-author-end	59)
(defconst books-tab-title	60)
(defconst books-tab-title-end	131)
(defconst books-tab-date	132)
(defconst books-tab-date-end	137)
(defconst books-tab-pages	138)
(defconst books-tab-pages-end	142)
(defconst books-tab-media	143)
(defconst books-tab-media-end	150)
(defconst books-tab-subject	151)
(defconst books-tab-subject-end	158)
(defconst books-tab-rating	159)
(defconst books-tab-rating-end	162)
(defconst books-tab-library	163)
(defconst books-tab-library-end	166)
(defconst books-tab-series	167)
(defconst books-tab-series-c	167)
(defconst books-tab-series-d	171)
(defconst books-tab-series-f	175)
(defconst books-tab-series-b	179)
(defconst books-tab-series-v	183)
(defconst books-tab-series-publ	202)
(defconst books-tab-series-end	212)
(defconst books-tab-comment	213)
(defconst books-tab-comment-end	999)

(defvar books-tab-stop-list
  '(0 60 132 139 143 151 160 167 213)
  "*List of tab stop positions used by tab-to-tab-stops.")

(defvar books-tab-stop-list-rev
  (reverse books-tab-stop-list)
  "*List of tab stop positions used by tab-to-tab-stops.")

(defvar books-lowercase-words
  (list "to" "and" "a" "the")
  "*Words that should be lower case inside titles.")

(defvar books-predicate-remove
  "^\\(to\\|and\\|a\\|the\\)\\>"
  "*Words to remove in title beginnings.")

(defvar books-mode-abbrev-table nil
  "Abbrev table in use in books-mode buffers.")
(define-abbrev-table 'books-mode-abbrev-table ())

(defvar books-directory (expand-file-name "~/docs/Books/")
  "Directory with book files")

(defvar books-mode-hook nil
  "Run at the very end of books-mode.")

(defvar books-mode-map nil
  "Keymap used in BOOK mode.")
(setq books-mode-map (make-sparse-keymap))
(define-key books-mode-map "\M-f" 'books-forward-to-tab-stop)
(define-key books-mode-map "\M-b" 'books-backward-to-tab-stop)
(define-key books-mode-map "\C-x\C-y" 'books-forward-to-insert-pt)
(define-key books-mode-map "\C-x#" 'books-form-id-manual)

;;;; Parsing variables
(defvar books-line-space nil "Whole book-length line of spaces")
(defvar books-shell-que nil "List of strings to be sent, FIFO")
(defvar books-shell-buffer nil "Buffer containing comint kermit session")
(defvar books-shell-lastin nil "Last string received from kermit")

;;;; FUNCTIONS


(defun books-mode ()
  "Major mode for editing BOOK list.

Turning on BOOK mode calls the value of the variable books-mode-hook with no args,
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map books-mode-map)
  (setq major-mode 'books-mode)
  (setq mode-name "Books")
  (setq local-abbrev-table books-mode-abbrev-table)
  (setq truncate-lines t)
  (overwrite-mode 1)
  (auto-save-mode -1)
  (run-hooks 'books-mode-hook))

(defun books-forward-to-tab-stop ()
  "Move cursor forward to next defined tab-stop column.
The variable books-tab-stop-list is a list of columns at which there are tab stops."
  (interactive)
  (if abbrev-mode (expand-abbrev))
  (let ((tabs books-tab-stop-list))
    (while (and tabs (>= (current-column) (car tabs)))
      (setq tabs (cdr tabs)))
    (if tabs
	(if (> (car tabs)
	       (save-excursion
		 (end-of-line)
		 (current-column)))
	    (beginning-of-line)
	  (move-to-column (car tabs)))
      (beginning-of-line))))

(defun books-backward-to-tab-stop ()
  "Move cursor backward to next defined tab-stop column.
The variable books-tab-stop-list is a list of columns at which there are tab stops."
  (interactive)
  (if abbrev-mode (expand-abbrev))
  (let ((tabs books-tab-stop-list-rev))
    (while (and tabs (<= (current-column) (car tabs)))
      (setq tabs (cdr tabs)))
    (if tabs
	(move-to-column (car tabs))
      (beginning-of-line))))

;;----------------------------------------------------------------------
;; Function

(defsubst books-dethe (a)
  (books-string-replace-matches "\\<\\(a\\|the\\) " "" nil nil a))

(defun books-string< (a b)
  "Is a < b ignoring The/A"
  (setq a (books-dethe a)
	b (books-dethe b))
  (string< a b))

(defun books-forward-to-insert-pt ()
  "Find the alphabetical place to insert the line in the paste buffer."
  (interactive)
  (let* ((ln (current-kill 0))
	 (lnkey (books-dethe ln)))
    (beginning-of-line)
    (cond ((equal lnkey ln)
	   (while (not (string<
			(buffer-substring (point) (save-excursion (end-of-line) (point)))
			lnkey))
	     (forward-line -1))
	   (while (string<
		   (buffer-substring (point) (save-excursion (end-of-line) (point)))
		   lnkey)
	     (forward-line 1)))
	  ;; No the, faster compares
	  (t
	   (while (not (books-string<
			(buffer-substring (point) (save-excursion (end-of-line) (point)))
			lnkey))
	     (forward-line -1))
	   (while (books-string<
		   (buffer-substring (point) (save-excursion (end-of-line) (point)))
		   lnkey)
	     (forward-line 1))))
    (insert ln)
  ))

;;----------------------------------------------------------------------
;; Hilit

(defun books-hilit-colrange (col-list)
  "looks for title and returns (start . end)."
  (let (en
	(start-col (car col-list))
	(end-col (cdr col-list)))
    (while (and (not en) (not (eobp)))
      (move-to-column start-col)
      (move-to-column end-col)
      (unless (eolp)
	(setq en (cons (- (point) (- end-col start-col))
		       (point))))
      (beginning-of-line 2)
      )
    en))

(cond ((featurep 'hilit19)
       (hilit-set-mode-patterns
	'(books-mode)
	'(("^[ \t-].*$" nil comment)
	  (books-hilit-colrange ( 0 . 58 ) defun)
	  (books-hilit-colrange ( 60 . 130 ) comment)
	  (books-hilit-colrange ( 167 . 212 ) string)
	))))

;;----------------------------------------------------------------------
;;;
;;; Toread Parsing
;;;

(defun books-strip (author)
  "Return first author/title, cleaned up"
  (if (string-match "(" author)
      (setq author (substring author 0 (match-beginning 0))))
  (if (string-match " *$" author)
      (setq author (substring author 0 (match-beginning 0))))
  )

(defun books-form-id (author title linenum)
  (let (id)
    (setq id (concat (substring author 0 (min 5 (length author)))
		     (if (string-match ", \\(...\\)" author)
			 (concat "," (match-string 1 author))
		       "")
		     "@"
		     (substring title 0 (min 5 (length title)))
		     ;;(substring line books-tab-title (+ 5 books-tab-title))
		     ))
    (while (string-match " +" title)
      (setq title (substring title (match-end 0))
	    id (concat id (substring title 0 1))))
    (setq id (books-string-replace-matches "'" "" nil nil id))
    (concat id)))
;;(books-form-id "wilson, snyder" "This is the title" 222)

(defun books-form-id-manual ()
  "Print the Book id of the current line.  Also put into the yank buffer."
  (interactive)
  (save-excursion
    (let (line author title media id linenum blist)
      ;;
      (beginning-of-line)
      (setq line   (buffer-substring (point) (save-excursion (end-of-line) (point)))
	    author (books-strip (substring line books-tab-author books-tab-author-end))
	    title  (books-strip (substring line books-tab-title  books-tab-title-end))
	    linenum 0
	    id     (books-form-id author title linenum))
      (message id)
      (kill-new (concat "1>> BOOKID " id "\n"))
      )))

;;----------------------------------------------------------------------
;; Line insertion assistance

(defun books-capitalize (strg)
  (let (newstrg)
    (while (string-match " *\\([^ ]+\\)\\(.*\\)$" strg)
      (let ((word (downcase (match-string 1 strg))))
	(setq newstrg (concat newstrg
			      (if newstrg " " "")
			      (if (and newstrg (member word books-lowercase-words))
				  word
				(capitalize (match-string 1 strg))))
	      strg (match-string 2 strg))))
    newstrg))
;;(books-capitalize "TO THE CITY TO THE WOODS")

(defun books-author-lastname-first (strg)
  (setq strg (books-string-replace-matches "[,\\.]" "" nil nil strg))
  (if (string-match "\\(.*\\) \\([^ ]*\\)$" strg)
      (setq strg (concat (match-string 2 strg) ", " (match-string 1 strg))))
  strg)
;;(books-author-lastname-first "Kurt, Jr. Vonnegut")

(defun books-insert-at-column (col colend text)
  (while (< (current-column) col)
    (insert " "))
  (insert text))

(defun books-insert-line (author title date pages media subject rating library dec series publisher comment)
  "Insert a single line in book format."
  (insert "\n")
  (books-insert-at-column books-tab-author	books-tab-author-end	author)
  (books-insert-at-column books-tab-title	books-tab-title-end	title)
  (books-insert-at-column books-tab-date	books-tab-date-end	date)
  (books-insert-at-column books-tab-pages	books-tab-pages-end	pages)
  (books-insert-at-column books-tab-media	books-tab-media-end	media)
  (books-insert-at-column books-tab-subject	books-tab-subject-end	subject)
  (books-insert-at-column books-tab-rating	books-tab-rating-end	rating)
  (books-insert-at-column books-tab-library	books-tab-library-end	library)
  (books-insert-at-column books-tab-series-d	books-tab-series-f	dec)
  (books-insert-at-column books-tab-series-publ	books-tab-series-end	publisher)
  (books-insert-at-column books-tab-comment	books-tab-comment-end	comment)
  (insert "\n")
  )

;;----------------------------------------------------------------------

(defun books-string-replace-matches (from-string to-string fixedcase literal string)
  "Replace occurances of from-string with to-string in the string."
  (while (string-match from-string string)
      (setq string (replace-match to-string fixedcase literal string)))
  string)

;;----------------------------------------------------------------------

