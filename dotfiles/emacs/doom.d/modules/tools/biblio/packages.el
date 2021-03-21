;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "ca09076c3d6e51cc4ffe208c8176fccf3710fcc6")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "ca09076c3d6e51cc4ffe208c8176fccf3710fcc6"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "ca09076c3d6e51cc4ffe208c8176fccf3710fcc6"))
(when (featurep! :lang org)
  (package! org-ref :pin "7b0ebdd2e34b3a509c7f043cb9d919a0069491c2"))
(when (featurep! :lang org +roam)
  (package! org-roam-bibtex :pin "81b6fedf99996a78199067e61935964dea9389ee07447ecf7a41fcd5b7abc40f4c3c"))
