;;; ~/.doom.d/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun user/cider-eval-and-run-ns-tests ()
  "Eval and run namespace tests"
  (interactive)
  (cider-eval-buffer)
  (cider-test-run-ns-tests t))

;;;###autoload
(defun user/cider-eval-and-run-test ()
  "Eval and run defun at point"
  (interactive)
  (cider-eval-defun-at-point)
  (cider-test-run-test))

;;;###autoload
(defun user/cider-read-eval-and-call-defun-at-point ()
  "Eval and call form at point"
  (interactive)
  (let ((inhibit-message t))
    (cider-eval-defun-at-point)
  (cider-read-and-eval-defun-at-point)))
