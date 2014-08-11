(defun x ()
  (interactive)
  (global-delete-matching-lines "/[^./]*$")
  (global-delete-matching-lines ",v$")
  (global-delete-matching-lines "\\.\\(b64\\|gph\\|dis\\|laz\\|jpg\\|ind\\)$")
  (global-delete-matching-lines "\\.\\(gif\\|grf\\|tax\\|jpeg\\|inc\\|frm\\|out\\)$")
  (global-delete-matching-lines "\\.\\(asm\\|lst\\|obj\\)$")
  (global-delete-matching-lines "\\.\\(v\\|h\\|c\\|f\\|1\\|a\\|o\\)$")
  (global-delete-matching-lines "\\.\\(tsf\\|tsfpp\\|aloe\\|body\\)$")
  (global-delete-matching-lines "\\.\\(exe\\|bat\\|src\\|lnk\\|xls\\)$")
  (global-delete-matching-lines "\\.\\(tex\\|fm\\|s\\|b2s\\)$")
  (global-delete-matching-lines "\\.\\(exe\\|bat\\|src\\|lnk\\|xls\\)$")
  (global-delete-matching-lines "\\.\\(64prg\\|bas\\|rmail\\|m\\)$")
  (global-delete-matching-lines "\\.\\(pas\\|dat\\|esy64\\|p\\|txt\\)$")
  (global-delete-matching-lines "\\.\\(el\\|wks\\)$")
  (global-delete-matching-lines "\\.\\(doc\\|txt\\|cat\\)$")
  )

(defun perdowncase (basea baseb dir)
    (let ((a (concat dir basea))
          (b (concat dir (downcase baseb))))
      (cond ((not (equal a b))
             (message a)
             (rename-file a "__TMP__")
	     (rename-file "__TMP__" b)))))



(defun perext (basea baseb dir)
    (let ((a (concat dir basea))
          (b (concat dir baseb)))
      (setq b (verilog-string-replace-matches "\\." "_" nil nil a))
      (setq b (verilog-string-replace-matches "_doc$" ".doc" nil nil b))
      (setq b (verilog-string-replace-matches "_prn$" ".prn" nil nil b))
      (setq b (verilog-string-replace-matches "_pic$" ".pic" nil nil b))
      (setq b (verilog-string-replace-matches "_txt$" ".txt" nil nil b))
      (setq b (verilog-string-replace-matches "_wks$" ".wks" nil nil b))
      (setq b (verilog-string-replace-matches "_grf$" ".grf" nil nil b))
      (setq b (verilog-string-replace-matches "_gif$" ".gif" nil nil b))
      (setq b (verilog-string-replace-matches "_p$" ".p" nil nil b))
      (setq b (verilog-string-replace-matches "_s$" ".s" nil nil b))
      (setq b (verilog-string-replace-matches "_LHA$" ".LHA" nil nil b))
      (setq b (verilog-string-replace-matches "_c$" ".c" nil nil b))
      (setq b (verilog-string-replace-matches "_prg$" ".prg" nil nil b))
      (setq b (verilog-string-replace-matches "_h$" ".h" nil nil b))
      (setq b (verilog-string-replace-matches "_f$" ".f" nil nil b))
      (setq b (verilog-string-replace-matches "_fm$" ".fm" nil nil b))
      (cond ((not (equal a b))
             (message a)
             (rename-file a b)))))

(defun detxt (basea baseb dir)
    (let ((a (concat dir basea))
          (b (concat dir baseb)))
      (setq b (verilog-string-replace-matches ".txt$" "" nil nil b))
      (cond ((not (equal a b))
             (message a)
             (rename-file a b)))))
