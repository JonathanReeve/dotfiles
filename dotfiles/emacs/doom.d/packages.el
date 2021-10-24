;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! flycheck-vale)

(package! pdf-tools :built-in 'prefer)

;; Read epubs
(package! nov)

;; Change themes with PyWal
(package! ewal-doom-themes)

;; Query Org files
(package! org-ql)

;; Make eshell better
(package! fish-completion)

(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

(package! emacsql-sqlite)
