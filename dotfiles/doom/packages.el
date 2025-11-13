;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! flycheck-vale)

(package! pdf-tools :built-in 'prefer)

;; Read epubs
(package! nov)

;; See https://github.com/fuxialexander/org-pdftools
;; (package! org-pdftools)

;; Use BibSonomy
;; (package! biblio-bibsonomy)

;; Change themes with PyWal
(package! ewal-doom-themes)

;; Query Org files
;; (package! org-ql)

;; Make eshell better
;; (package! fish-completion)

;; Edit Turtle files
;; (package! ttl-mode)

(package! websocket)
(package! pass)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

(package! org-ref)
(package! org-roam-bibtex
 :recipe (:host github :repo "org-roam/org-roam-bibtex"))

;; (package! org-cite-csl-activate :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate" :files ("*.el")))

(package! org-clock-reminder
  :recipe (:host github :repo "inickey/org-clock-reminder"))

;; UI improvements for Org mode
(package! org-modern)

;; (package! vulpea :recipe (:host github :repo "d12frosted/vulpea"))

;; (package! emacsql-sqlite)

;; Pollen, the document processing language for Racket
(package! pollen-mode)

;; Scribble, the language Pollen is written in
(package! scribble-mode)


;; Web scraping
;; (package! enlive)

;; Life in the danger zone!
;; (unpin! org-ref org-roam-bibtex)

;; (package! notebook-mode :recipe (:host github :repo "rougier/notebook-mode" :files ("*.el")))

;; ~/.doom.d/packages.el
;; (package! evil-colemak-basics) ; colemak remaps

(package! quarto-mode :recipe (:host github :repo "quarto-dev/quarto-emacs" ))

;; (package! org-msg
;;   :recipe (:host github :repo "danielfleischer/org-msg" :branch "1.12")
;;   :pin "4dcd70f"
;;  )

;; (package! org-typst-preview :recipe (:host github :repo "remimimimimi/org-typst-preview.el"))
 
(package! shell-maker
  :recipe (:host github :repo "xenodium/shell-maker")
  :pin "9388492f6cb56e52a56fbdf6d5fe68c1cdff4909")
(package! acp :recipe (:host github :repo "xenodium/acp.el")
  :pin "f9dd828c7bfb209523e907cc947668924ff9f1bf")
(package! agent-shell :recipe (:host github :repo "xenodium/agent-shell")
  :pin "203a782087b2e2f40a7368022fdae24ed84bccc8")

(package! aider :recipe (:host github :repo "tninja/aider.el" )
   :pin "7053d21eb29407f48bc3b50a9b472feeae9d562c")

