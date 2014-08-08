;;; prof.el -- Simple profiling shell
(require 'elp)
(defun x ()
  (interactive)
  (save-excursion
    ;; Make sure we byte compile what we are profiling
    (set-buffer "prof.el")
    (save-buffer)
    (byte-compile-file (buffer-file-name))
    (load-file (concat (buffer-file-name) "c")))
  ;;
  (elp-reset-all)
  (elp-restore-all)
  (x-real)
  (elp-results)
  )

(defun x-real ()
  ;; What to do
  (elp-instrument-package "dinotrace")
  (elp-instrument-package "verilog")
  (find-file "~/WorkArea_elsy/elsy/rtl/css/css.v")
  ;;(revert-buffer t t nil)
  (dinotrace-unannotate-buffer)
  (dinotrace-update)
  )
