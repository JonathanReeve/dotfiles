;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! flycheck-vale)

;; (package! ox-ipynb :recipe (:host github :repo "jkitchin/ox-ipynb"))

(package! pdf-tools :built-in 'prefer)

;; Read epubs
(package! nov)

;; Use BibSonomy
(package! biblio-bibsonomy)

;; Change themes with PyWal
(package! ewal-doom-themes)

;; Query Org files
(package! org-ql)
