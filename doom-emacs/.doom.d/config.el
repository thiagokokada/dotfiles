;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Lispyvile
(after! 'lispyville
  (lispyville-set-key-theme
   '(additional
     additional-insert
     (additional-movement normal visual motion)
     additional-wrap
     commentary
     (escape insert emacs)
     operators
     prettify
     slurp/barf-cp)))

(add-hook! 'clojure-mode-hook #'lispyville-mode)
(add-hook! 'emacs-lisp-mode-hook #'lispyville-mode)
(add-hook! 'hy-mode-hook #'lispyville-mode)
(add-hook! 'lisp-mode-hook #'lispyville-mode)
(add-hook! 'prog-mode-hook #'(lambda ()
                               (dtrt-indent-mode)
                               (dtrt-indent-adapt)))

;; Evil-numbers
(define-key evil-normal-state-map (kbd "<kp-add>") #'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "<kp-subtract>") #'evil-numbers/dec-at-pt)

;; Zoom
(global-set-key (kbd "C-0") #'zoom-frm-unzoom)
(global-set-key (kbd "C-=") #'zoom-frm-in)
(global-set-key (kbd "C--") #'zoom-frm-out)
