;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;;; GENERAL

;; theme
(require 'doom-themes)
(load-theme 'doom-spacegrey t)
(doom-themes-neotree-config)
(doom-themes-org-config)

;; modeline
(setq doom-modeline-major-mode-icon t)

;; disable confirmation message on exit
(setq confirm-kill-emacs nil)

;; font
(setq doom-font (font-spec :family "Hack" :size 14)
      doom-big-font (font-spec :family "Hack" :size 18))

(add-hook! 'after-make-frame-functions
  (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Font Awesome 5 Free") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Font Awesome 5 Brands") nil 'append))

;; set localleader the same as Spacemacs
(setq doom-localleader-key ",")

;; move betweeen windows faster
(map!
 (:map evil-normal-state-map
   ("C-h" #'evil-window-left
    "C-j" #'evil-window-down
    "C-k" #'evil-window-up
    "C-l" #'evil-window-right))
 (:map evil-insert-state-map
   ("C-h" #'backward-char
    "C-j" #'next-line
    "C-k" #'previous-line
    "C-l" #'forward-char)))

;; dired
(map! (:n "-" #'dired-jump))

;; neotree
(setq doom-neotree-file-icons t)
(map!
 (:g "C-x t" #'neotree)
 (:n "0" #'neotree-toggle))

;; which-key
(setq which-key-idle-delay 0.1)

;; make ESC to work as expected in minibuffers
(map!
 (:map minibuffer-local-map [escape] #'minibuffer-keyboard-quit)
 (:map minibuffer-local-ns-map [escape] #'minibuffer-keyboard-quit)
 (:map minibuffer-local-completion-map [escape] #'minibuffer-keyboard-quit)
 (:map minibuffer-local-must-match-map [escape] #'minibuffer-keyboard-quit)
 (:map minibuffer-local-isearch-map [escape] #'minibuffer-keyboard-quit)
 (:g [escape] #'keyboard-quit))

;;; MODULES

;; company
(setq company-selection-wrap-around t)
(set-company-backend! :derived 'prog-mode
  'company-files
  'company-keywords)
(set-company-backend! :derived 'text-mode
  'company-files
  'company-emoji)

;; dtrt-indent
(after! dtrt-indent
  (add-hook! prog-mode #'dtrt-indent-adapt))

;; projectile
(add-hook! projectile-mode
  (map!
   (:leader
     (:map projectile-mode-map
       (:prefix ("p" . "project")
         :desc "Toggle between implementation and test"
         "a" #'projectile-toggle-between-implementation-and-test
         :desc "Replace using regexp"
         "e" #'projectile-replace-regexp)))))

;; rainbow
(add-hook! prog-mode #'rainbow-mode) ; Colorize hex color strings

;; whitespace
(setq whitespace-line-column 80 ; Highlight lines longer than 80 chars
      whitespace-style '(face lines-tail))

(add-hook! prog-mode #'whitespace-mode)

;;; LANGUAGES

;; clojure
(add-hook! clojure-mode
  (map!
   (:map clojure-mode-map
     (:n "gd" #'cider-find-var)
     (:localleader
       ("a" #'clojure-align)
       (:prefix ("e" . "eval")
         "b" #'cider-eval-buffer
         "f" #'cider-eval-sexp-at-point)
       (:prefix ("r" . "repl")
         "'" #'cider-connect
         "\"" #'cider-connect-cljs
         "o" #'cider-find-and-clear-repl-output)))))

(add-hook! cider-mode #'emidje-setup)

(add-hook! cider-repl-mode
  (map!
   (:localleader
     (:map cider-repl-mode-map
       ("c" #'cider-repl-clear-buffer
        "R" #'cider-restart
        "r" #'cider-ns-refresh
        "q" #'cider-quit)))))

;; elisp
(add-hook! emacs-lisp-mode
  (map!
   (:map emacs-lisp-mode-map
     (:localleader
       "e" nil ; Unmap macrostep-expand
       "x" #'macrostep-expand
       :desc "REPL"
       "r" #'+emacs-lisp/open-repl
       (:prefix ("e" . "eval")
         "b" #'eval-buffer
         "d" #'eval-defun
         "r" #'eval-region)))))

;; eshell
(add-hook! eshell-mode
  (setenv "TERM" "xterm-256color")
  (setq xterm-color-preserve-properties t
        eshell-preoutput-filter-functions '(xterm-color-filter)
        eshell-output-filter-functions (remove #'eshell-handle-ansi-color
                                               eshell-output-filter-functions)))

;; markdown
(add-hook! markdown-mode
  (map!
   (:map markdown-mode-map
     (:localleader
       ("f" #'flymd-flyit)))))

;;; CUSTOM PACKAGES

;; evil-lisp-state
(def-package! evil-lisp-state
  :init (setq evil-lisp-state-global t)
  :config (evil-lisp-state-leader "`"))

;; lispyville
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
     (additional-movement normal visual motion)
     c-w
     (commentary normal visual)
     (escape insert emacs)
     (operators normal)
     prettify
     slurp/barf-cp)))

;; uuidgen-el
(def-package! uuidgen
  :config
  (map!
   (:leader
     (:prefix ("i" . "insert")
       :desc "Insert UUIDv4"
       "u" #'uuidgen))))

;; zoom-frm
(def-package! zoom-frm
  :config
  (map!
   ("C-=" #'zoom-frm-in
    "C--" #'zoom-frm-out
    "C-0" #'zoom-frm-unzoom))
  (global-set-key (vector (list #'control mouse-wheel-down-event)) #'zoom-frm-in)
  (global-set-key (vector (list #'control mouse-wheel-up-event)) #'zoom-frm-out))

;;; MISC

;; load local configuration file if exists
(load! "local.el" "~/.doom.d" t)
