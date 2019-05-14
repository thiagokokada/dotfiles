;;; init.el -*- lexical-binding: t; -*-

;; increase garbage collection threshold
(setq gc-cons-threshold 50000000)

;; enable use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; disable distractions
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

;; enable line and column eyecandy
(global-hl-line-mode +1)
(line-number-mode +1)
(global-display-line-numbers-mode 1)
(column-number-mode t)
(size-indication-mode t)

;; disable startup screen
(setq inhibit-startup-screen t)

;; set fonts
(set-frame-font (font-spec :family "Hack" :size 14) nil t)

;; packages

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (global-company-mode))

(use-package counsel
  :ensure t)

(use-package dired
  :after evil
  :config
  (evil-define-key 'normal nil "-" #'dired-jump))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-dracula t))

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (global-flycheck-mode))

(use-package general
  :ensure t
  :config
  (general-evil-setup t))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package lispyville
  :ensure t
  :hook ((common-lisp-mode . lispyville-mode)
         (emacs-lisp-mode . lispyville-mode)
         (scheme-mode . lispyville-mode)
         (racket-mode . lispyville-mode)
         (hy-mode . lispyville-mode)
         (lfe-mode . lispyville-mode)
         (clojure-mode . lispyville-mode))
  :when (display-graphic-p) ;; lispyville breaks terminal Emacs
  :config
  (lispyville-set-key-theme
   `(additional
     additional-insert
     (additional-movement normal visual motion)
     (additional-wrap normal insert)
     c-w
     (commentary normal visual)
     (escape insert emacs)
     (operators normal)
     prettify
     slurp/barf-cp)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode +1))

(use-package smartparens
  :ensure t
  :hook ((prog-mode . smartparens-mode)
	 (text-mode . smartparens-mode)))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode +1))

;; keybindings
(general-nmap
 "C-h" #'evil-window-left
 "C-j" #'evil-window-down
 "C-k" #'evil-window-up
 "C-l" #'evil-window-right)

(general-create-definer leader-map
  :prefix "SPC"
  :keymaps '(normal visual))

(leader-map
  "/" #'counsel-rg
  "SPC" #'counsel-find-file)
