;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Theme
(require 'doom-themes)
(load-theme 'doom-spacegrey t)
(doom-themes-neotree-config)
(doom-themes-org-config)

;; Disable confirmation message on exit
(setq confirm-kill-emacs nil)

;; Font
(setq doom-font (font-spec :family "Hack" :size 14)
      doom-big-font (font-spec :family "Hack" :size 18))

;; Set localleader the same as Spacemacs
(setq doom-localleader-key ",")

;; Which-key
(setq which-key-idle-delay 0.1)

;; Move betweeen windows faster
(map! :map global-map
      "C-h" #'evil-window-left
      "C-j" #'evil-window-down
      "C-k" #'evil-window-up
      "C-l" #'evil-window-right)

;; Dired
(define-key evil-normal-state-map (kbd "-") #'dired-jump)

;; Neotree
(define-key evil-normal-state-map (kbd "C-x t") #'neotree)

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

(add-hook! (clojure-mode
            common-lisp-mode
            emacs-lisp-mode
            hy-mode
            lfe-mode
            racket-mode
            scheme-mode) #'lispyville-mode)

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
(after! dtrt-indent
  (add-hook! (prog-mode text-mode) #'dtrt-indent-adapt))

;; Undo-tree
(after! undo-tree
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;; Load Nubank configuration
(let ((nudev-emacs-path "~/dev/nu/nudev/ides/emacs/nu.el"))
  (when (file-exists-p nudev-emacs-path)
    (load nudev-emacs-path)
    (require 'nu)))
