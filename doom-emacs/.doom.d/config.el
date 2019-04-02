;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;;; GENERAL

;; Theme
(require 'doom-themes)
(load-theme 'doom-spacegrey t)
(doom-themes-neotree-config)
(doom-themes-org-config)

;; Modeline
(setq doom-modeline-major-mode-icon t)

;; Disable confirmation message on exit
(setq confirm-kill-emacs nil)

;; Font
(setq doom-font (font-spec :family "Hack" :size 14)
      doom-big-font (font-spec :family "Hack" :size 18))

;; Set localleader the same as Spacemacs
(setq doom-localleader-key ",")

;; Move betweeen windows faster
(map!
 (:n
  "C-h" #'evil-window-left
  "C-j" #'evil-window-down
  "C-k" #'evil-window-up
  "C-l" #'evil-window-right)
 (:map evil-insert-state-map
  "C-h" #'backward-char
  "C-j" #'next-line
  "C-k" #'previous-line
  "C-l" #'forward-char))

;; Highlight lines longer than 80 chars
(setq whitespace-line-column 80
      whitespace-style '(face lines-tail))

(add-hook! prog-mode #'whitespace-mode)

;; Dired
(map! (:n "-" #'dired-jump))

;; Neotree
(setq doom-neotree-file-icons t)
(map!
 (:g "C-x t" #'neotree)
 (:n "0" #'neotree-toggle))

;; Which-key
(setq which-key-idle-delay 0.1)

;; Undo-tree
(setq undo-tree-auto-save-history t
      undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; Make ESC to work as expected in minibuffers
(map!
 (:map minibuffer-local-map [escape] #'minibuffer-keyboard-quit)
 (:map minibuffer-local-ns-map [escape] #'minibuffer-keyboard-quit)
 (:map minibuffer-local-completion-map [escape] #'minibuffer-keyboard-quit)
 (:map minibuffer-local-must-match-map [escape] #'minibuffer-keyboard-quit)
 (:map minibuffer-local-isearch-map [escape] #'minibuffer-keyboard-quit)
 (:g [escape] #'keyboard-quit))

;;; AFTER MODULE

;; Dtrt-indent
(after! dtrt-indent
  (add-hook! (prog-mode text-mode) #'dtrt-indent-adapt))

;; Ivy
(after! ivy
  (map!
   (:leader
     (:prefix ("/" . "search")
       :desc "Search thing at point in git project"
       "*" #'+misc/search-thing-at-point))))

;;; HOOKS

;; Projectile
(add-hook! projectile-mode
  (map!
   (:leader
     (:map projectile-mode-map
       (:prefix ("p" . "project")
         :desc "Toggle between implementation and test"
         "a" #'projectile-toggle-between-implementation-and-test
         :desc "Replace using regexp"
         "e" #'projectile-replace-regexp)))))

;; Clojure
(add-hook! clojure-mode
  (map!
   (:map clojure-mode-map
     (:n "gd" #'cider-find-var)
     (:localleader
       (:prefix ("e" . "eval")
         "b" #'cider-eval-buffer
         "f" #'cider-eval-sexp-at-point)
       (:prefix ("r" . "repl")
         "'" #'cider-connect
         "\"" #'cider-connect-cljs
         "o" #'cider-find-and-clear-repl-output)))))

(add-hook! cider-repl-mode
  (map!
   (:localleader
     (:map cider-repl-mode-map
       ("c" #'cider-repl-clear-buffer
        "R" #'cider-restart
        "r" #'cider-ns-refresh
        "q" #'cider-quit)))))

;; Eshell
(add-hook! eshell-mode
  (setenv "TERM" "xterm-256color")
  (setq xterm-color-preserve-properties t
        eshell-preoutput-filter-functions '(xterm-color-filter)
        eshell-output-filter-functions (remove #'eshell-handle-ansi-color
                                               eshell-output-filter-functions)))

;;; CUSTOM PACKAGES

;; Lispyvile
(def-package! lispyville
  :hook ((common-lisp-mode . lispyville-mode)
         (emacs-lisp-mode . lispyville-mode)
         (scheme-mode . lispyville-mode)
         (racket-mode . lispyville-mode)
         (hy-mode . lispyville-mode)
         (lfe-mode . lispyville-mode)
         (clojure-mode . lispyville-mode))
  :config
  (lispyville-set-key-theme
   '(additional
     additional-insert
     (additional-movement normal visual motion)
     (additional-wrap normal insert)
     c-w
     (commentary normal visual)
     (escape insert emacs)
     (operators normal)
     prettify
     slurp/barf-cp)))

;; Uuidgen
(def-package! uuidgen
  :config
  (map!
   (:leader
     (:prefix ("i" . "insert")
       :desc "Insert UUIDv4"
       "u" #'uuidgen))))

;; Zoom-frm
(def-package! zoom-frm
  :config
  (map!
   (:g
    "C-=" #'zoom-frm-in
    "C--" #'zoom-frm-out
    "C-0" #'zoom-frm-unzoom))
  (global-set-key (vector (list #'control mouse-wheel-down-event)) #'zoom-frm-in)
  (global-set-key (vector (list #'control mouse-wheel-up-event)) #'zoom-frm-out))

;;; MISC

;; Load local configuration file if exists
(load! "local.el" "~/.doom.d" t)
