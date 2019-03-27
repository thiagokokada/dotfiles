;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Theme
(load-theme 'doom-spacegrey t)
(setq doom-theme 'doom-spacegrey)
; (doom-themes-visual-bell-config)
(doom-themes-neotree-config)
(doom-themes-org-config)

;; Font
(setq doom-font (font-spec :family "Hack" :size 15))

;; Set localleader the same as Spacemacs
(setq doom-localleader-key ",")

;; Which-key
(setq which-key-idle-delay 0.1)

;; Move betweeen windows faster
(global-set-key (kbd "C-h") #'evil-window-left)
(global-set-key (kbd "C-j") #'evil-window-down)
(global-set-key (kbd "C-k") #'evil-window-up)
(global-set-key (kbd "C-l") #'evil-window-right)

;; Make ESC to work as expected in minibuffers
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Lispyvile
(after! lispyville
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

(add-hook! clojure-mode #'lispyville-mode)
(add-hook! emacs-lisp-mode #'lispyville-mode)
(add-hook! hy-mode #'lispyville-mode)
(add-hook! lisp-mode #'lispyville-mode)

;; Clojure
(add-hook! clojure-mode
           (map!
             (:localleader
               (:map clojure-mode-map
                 (:prefix ("e" . "eval")
                   "b" #'cider-eval-buffer
                   "f" #'cider-eval-sexp-at-point))
           (evil-define-key 'normal clojure-mode-map "gd" #'cider-find-var))))

;; Dtrt-indent
; (dtrt-indent-global-mode t)
; (add-hook! prog-mode #'dtrt-indent-adapt)
; (add-hook! text-mode #'dtrt-indent-adapt)

;; Undo-tree
(global-undo-tree-mode t)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; Evil-numbers
(define-key evil-normal-state-map (kbd "<kp-add>") #'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "<kp-subtract>") #'evil-numbers/dec-at-pt)
