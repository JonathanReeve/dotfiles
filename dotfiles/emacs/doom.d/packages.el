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

;; (package! org-roam-server)

;; Org-roam v2 Stuff.
;; From [[https://org-roam.discourse.group/t/org-roam-bibtex-for-org-roam-v2/1574/70?u=jonathan][this comment]]:
;; (package! org-roam
;;   :recipe (:host github :repo "org-roam/org-roam" :branch "v2"))

;; (package! org-roam-bibtex
;;   :recipe (:host github :repo "org-roam/org-roam-bibtex" :branch "org-roam-v2"))

;; (unpin! bibtex-completion helm-bibtex ivy-bibtex)

;; ;; Python Requests-like package for elisp
;; ;; for querying APIs
;; (package! request)

;; Look things up in dictionaries
;; (package! dictionary)

;; Associate todo files with projects
;; (package! org-projectile)

;; Pollen, the document processing language for Racket
(package! pollen-mode)

;; Web scraping
(package! enlive)
