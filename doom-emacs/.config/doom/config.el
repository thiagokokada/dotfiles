;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;;; GENERAL

;; theme
(load-theme 'doom-vibrant t)

;; disable confirmation message on exit
(setq confirm-kill-emacs nil)

;; set window title with "[project] filename"
(setq frame-title-format
      (setq icon-title-format
            '(""
              (:eval
               (format "[%s] " (projectile-project-name)))
              "%b")))

;; font
(setq doom-font (font-spec :family "Hack" :size 18)
      doom-big-font-increment 4
      doom-unicode-font (font-spec :family "Noto Sans Mono"))

;; enable minibuffer to work correctly in evil mode
(setq evil-collection-setup-minibuffer t)

;; set localleader the same as Spacemacs
(setq doom-localleader-key ",")

;; general mappings
(map!
 ; remove default workspace shortcuts
 :n "C-t" nil
 :n "C-S-t" nil
 ; move betweeen windows faster in normal mode
 :m "C-h" #'evil-window-left
 :m "C-j" #'evil-window-down
 :m "C-k" #'evil-window-up
 :m "C-l" #'evil-window-right
 ; move windows faster in normal mode
 :m "C-S-h" #'+evil/window-move-left
 :m "C-S-j" #'+evil/window-move-down
 :m "C-S-k" #'+evil/window-move-up
 :m "C-S-l" #'+evil/window-move-right
 ; misc
 :n "-" #'dired-jump
 :nv "C-a" #'evil-numbers/inc-at-pt
 :nv "C-S-a" #'evil-numbers/dec-at-pt
 :nv "C-SPC" #'+fold/toggle)

;; ivy
(after! ivy
  (set-face-attribute
   'ivy-minibuffer-match-face-1 nil :foreground nil)
  (set-face-attribute
   'ivy-minibuffer-match-face-2 nil :background nil))

;; which-key
(setq which-key-idle-delay 0.4)

;;; MODULES

;; company
(setq company-selection-wrap-around t
      company-minimum-prefix-length 3
      company-idle-delay 0.4)

;; dired
(add-hook! dired-mode
  ;; Compress/Uncompress tar files
  (auto-compression-mode t)

  ;; Auto refresh buffers
  (global-auto-revert-mode t)

  ;; Also auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  ;; Emulate vinegar.vim
  (setq dired-omit-verbose nil)
  (setq dired-hide-details-hide-symlink-targets nil)
  (make-local-variable 'dired-hide-symlink-targets)
  (dired-hide-details-mode t))

;; projectile
(add-hook! projectile-mode
  (when (eq projectile-indexing-method 'alien)
    (setq projectile-enable-caching nil))
  (map!
   (:leader
     (:map projectile-mode-map
       (:prefix ("p" . "project")
         :desc "Find implementation or test in other window"
         "A" #'projectile-find-implementation-or-test-other-window
         :desc "Replace literal"
         "R" #'projectile-replace
         :desc "Replace using regexp"
         "X" #'projectile-replace-regexp)))))

;;; LANGUAGES

;; custom file extensions
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.repl\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.joker\\'" . clojure-mode))

;; clojure
(add-hook! clojure-mode
  (setq cljr-warn-on-eval nil
        cljr-eagerly-build-asts-on-startup nil
        cider-show-error-buffer 'only-in-repl)
  (map!
   (:map (clojure-mode-map clojurescript-mode-map)
    (:localleader
     ("=" #'clojure-align
      (:prefix ("n" . "namespace")
       "c" #'lsp-clojure-clean-ns)
      (:prefix ("e" . "namespace")
       "n" #'cider-eval-ns-form)
      "'" #'cider-jack-in-clj
      "\"" #'cider-jack-in-cljs
      "c" #'cider-connect-clj
      "C" #'cider-connect-cljs)))))

;; Workaround bug in completion
(after! cider-mode
  (add-hook #'company-completion-started-hook #'user/set-company-maps)
  (add-hook #'company-completion-finished-hook #'user/unset-company-maps)
  (add-hook #'company-completion-cancelled-hook #'user/unset-company-maps))

;; dart
(after! dart-mode
  (set-popup-rule! "\\*Hover\\*" :slot 2 :vslot -8 :select t :quit nil))

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

;; lsp
(after! lsp-mode
  (setq lsp-modeline-code-actions-mode t)
  (set-popup-rule! "\\*LSP Dart tests\\*" :slot 2 :vslot -8 :select t)
  (set-popup-rule! "^\\*lsp-" :slot 2 :vslot -8 :select t))

;; platuml
(add-hook! plantuml-mode
  (setq plantuml-output-type "txt")
  (set-popup-rule! "^\\*PLANTUML Preview\\*" :side 'right :width 0.5 :quit t))

;; python
(add-hook! python-mode
  (set-popup-rule! "^\\*format-all-errors\\*" :slot 2 :vslot -8 :select t))

;;; CUSTOM PACKAGES

;; hover
(use-package! hover
  :after dart-mode
  :config
  (setq hover-hot-reload-on-save t
        hover-screenshot-path "$HOME/Pictures"))

;; lispyville
(use-package! lispyville
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
     (atom-movement t)
     c-w
     (commentary normal visual)
     (escape insert emacs)
     (operators normal)
     prettify
     text-objects
     slurp/barf-cp)))

;; sort-words
(use-package! sort-words
  :defer t)

;;; MISC

;; load local configuration file if exists
(load! "local.el" "~/.config/doom" t)
