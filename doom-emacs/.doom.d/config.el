;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(with-eval-after-load 'lispyville
  (lispyville-set-key-theme
   '(additional
     additional-insert
     additional-movement
     additional-wrap
     commentary
     operators
     prettify
     slurp/barf-cp)))

(add-hook 'clojure-mode-hook #'lispyville-mode)
(add-hook 'emacs-lisp-mode-hook #'lispyville-mode)
(add-hook 'hy-mode-hook #'lispyville-mode)
(add-hook 'lisp-mode-hook #'lispyville-mode)

(global-set-key (kbd "C-0") #'zoom-frm-unzoom)
(global-set-key (kbd "C-=") #'zoom-frm-in)
(global-set-key (kbd "C--") #'zoom-frm-out)

(define-key evil-normal-state-map (kbd "<kp-add>") #'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "<kp-subtract>") #'evil-numbers/dec-at-pt)
