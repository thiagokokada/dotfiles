;;; ~/.doom.d/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun user/unset-company-maps (&rest unused)
  "Set default mappings (outside of company).
  Arguments (UNUSED) are ignored."
  (general-def
    :states 'insert
    :keymaps 'override
    "<up>" nil
    "<down>" nil
    "C-j" nil
    "C-k" nil
    "RET" nil
    [return] nil))

;;;###autoload
(defun user/set-company-maps (&rest unused)
  "Set maps for when you're inside company completion.
  Arguments (UNUSED) are ignored."
  (general-def
    :states 'insert
    :keymaps 'override
    "<down>" #'company-select-next
    "<up>" #'company-select-previous
    "C-j" #'company-select-next
    "C-k" #'company-select-previous
    "RET" #'company-complete
    [return] #'company-complete))

;;;###autoload
(defun user/cider-eval-and-run-ns-tests ()
  "Eval and run namespace tests"
  (interactive)
  (cider-load-buffer)
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
