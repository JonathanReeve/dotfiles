;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! flycheck-vale)

(package! pdf-tools :built-in 'prefer)

;; Read epubs
(package! nov)

;; Use BibSonomy
;; (package! biblio-bibsonomy)

;; Change themes with PyWal
(package! ewal-doom-themes)

;; Query Org files
(package! org-ql)

;; Make eshell better
(package! fish-completion)

;; Edit Turtle files
;; (package! ttl-mode)

(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

(package! org-ref)
(package! org-roam-bibtex
 :recipe (:host github :repo "org-roam/org-roam-bibtex"))

(package! vulpea :recipe (:host github :repo "d12frosted/vulpea"))

(package! emacsql-sqlite)

;; Pollen, the document processing language for Racket
(package! pollen-mode)

;; Web scraping
(package! enlive)

;; Life in the danger zone!
(unpin! org-ref org-roam-bibtex bibtex-actions)
