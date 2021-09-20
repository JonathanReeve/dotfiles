;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "12079bb09f203dda5cc2dd003bd60a6ad490f762")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "ca09076c3d6e51cc4ffe208c8176fccf3710fcc6"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "12079bb09f203dda5cc2dd003bd60a6ad490f762"))
(when (featurep! :lang org)
  (package! org-ref :pin "0d988807301bee68b0483f6b461125c31edfbc2c"))
(when (featurep! :lang org +roam2)
  (package! org-roam-bibtex :pin "c13a05b2c855ba1516241d8a1de33bf2c689d6e4"))
