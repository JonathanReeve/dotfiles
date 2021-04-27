;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "ca09076c3d6e51cc4ffe208c8176fccf3710fcc6")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "ca09076c3d6e51cc4ffe208c8176fccf3710fcc6"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "ca09076c3d6e51cc4ffe208c8176fccf3710fcc6"))
(when (featurep! :lang org)
  (package! org-ref :pin "3ca9beb744621f007d932deb8a4197467012c23a"))
(when (featurep! :lang org +roam)
  (package! org-roam-bibtex :pin "8d80bf980776df6ead53e917eb482ec8e309a1d76a78199067e61935964dea9389ee07447ecf7a41fcd5b7abc40f4c3c"))
