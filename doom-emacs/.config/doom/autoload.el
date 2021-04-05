;;; ~/.doom.d/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun user/screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

;; https://github.com/hlissner/doom-emacs/issues/4771#issuecomment-800428858
;;;###autoload
(defun +lookup--xref-show-fixed (fn identifier &optional show-fn)
   (let ((xrefs (funcall fn
                         (xref-find-backend)
                         identifier)))
     (when xrefs
       (funcall (or show-fn #'xref--show-defs)
                (lambda () xrefs)
                nil)
       'deferred)))
