;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)
(package! ag)
(package! lispyville)
(package! ripgrep)
(package! sort-words)
(package! uuidgen)

;; Zoom-frm
(package! frame-fns :recipe (:fetcher github :repo "emacsmirror/frame-fns"))
(package! frame-cmds :recipe (:fetcher github :repo "emacsmirror/frame-cmds"))
(package! zoom-frm :recipe (:fetcher github :repo "emacsmirror/zoom-frm"))
