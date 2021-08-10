;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "ca09076c3d6e51cc4ffe208c8176fccf3710fcc6")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "ca09076c3d6e51cc4ffe208c8176fccf3710fcc6"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "12079bb09f203dda5cc2dd003bd60a6ad490f762"))
(when (featurep! :lang org)
  (package! org-ref :pin "6a1fc880dcaa71a950fc0a5c565d0dfe749a96fe"))
(when (featurep! :lang org +roam)
  (package! org-roam-bibtex :pin "9c27f9c573274dc78b9655754ddb412c6f522f99"))
