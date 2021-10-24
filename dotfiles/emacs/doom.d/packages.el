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
;; (package! biblio-bibsonomy)

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

(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

(package! org-ref)
(package! org-roam-bibtex
 :recipe (:host github :repo "org-roam/org-roam-bibtex"))

(package! vulpea :recipe (:host github :repo "d12frosted/vulpea"))

(package! emacsql-sqlite)

;; (unpin! bibtex-completion helm-bibtex ivy-bibtex org-roam-bibtex org-roam)

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
