;;; ~/.doom.d/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun misc/cider-eval-and-run-ns-tests ()
  (interactive)
  (cider-eval-buffer)
  (cider-test-run-ns-tests t))

;;;###autoload
(defun misc/cider-eval-and-run-test ()
  (interactive)
  (cider-eval-defun-at-point)
  (cider-test-run-test))
