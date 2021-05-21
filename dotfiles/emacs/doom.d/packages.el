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

;; Fancy LISPy repl
;; (package! sly)

;; Make eshell better
(package! fish-completion)

;; Edit Turtle files
;; (package! ttl-mode)

(package! org-roam-server)

;; Python Requests-like package for elisp
;; for querying APIs
(package! request)

;; Look things up in dictionaries
;; (package! dictionary)

;; Associate todo files with projects
;; (package! org-projectile)

;; Pollen, the document processing language for Racket
(package! pollen-mode)
