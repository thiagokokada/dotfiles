;;; ~/.doom.d/autoload.el -*- lexical-binding: t; -*-

(defun ivy-with-thing-at-point (cmd)
  (let ((ivy-initial-inputs-alist
         (list
          (cons cmd (thing-at-point #'symbol)))))
    (funcall cmd)))

;;;###autoload
(defun +misc/search-thing-at-point ()
  "Search thing at point in git project."
  (interactive)
  (ivy-with-thing-at-point #'counsel-git-grep))
