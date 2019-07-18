;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)
(package! cider
  :recipe (:fetcher github
           :repo "clojure-emacs/cider"
           :commit "200c88adb1314b5811ba749af42ffb6817c1ca1b"
           :files ("*.el" (:exclude ".dir-locals.el"))
           :old-names (nrepl)))
(package! clj-refactor
  :recipe (:fetcher github
           :repo "clojure-emacs/clj-refactor.el"
           :commit "3d5d1fbf28bfcc00f917cd96d6784968dcbbc962"))
(package! flymd)
(package! lispyville)
(package! sort-words)
(package! uuidgen)
