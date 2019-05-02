;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)
(package! flymd)
(package! highlight-indentation)
(package! lispyville)
(package! sort-words)
(package! uuidgen)
(package! xterm-color)
(packages!
 (frame-fns :recipe (:fetcher github :repo "emacsmirror/frame-fns"))
 (frame-cmds :recipe (:fetcher github :repo "emacsmirror/frame-cmds"))
 (zoom-frm :recipe (:fetcher github :repo "emacsmirror/zoom-frm")))
