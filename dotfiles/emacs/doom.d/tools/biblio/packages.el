;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "ca09076c3d6e51cc4ffe208c8176fccf3710fcc6")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "ca09076c3d6e51cc4ffe208c8176fccf3710fcc6"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "ca09076c3d6e51cc4ffe208c8176fccf3710fcc6"))
(when (featurep! :lang org)
  (package! org-ref :pin "93f17b8a2cc9048a8df576f189cd5263662168db"))
(when (featurep! :lang org +roam)
  (package! org-roam-bibtex :pin "068d9c27650207447ecf7a41fcd5b7abc40f4c3c"))
