;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "bb47f355b0da8518aa3fb516019120c14c8747c9")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "bb47f355b0da8518aa3fb516019120c14c8747c9"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "bb47f355b0da8518aa3fb516019120c14c8747c9"))
(when (featurep! :completion vertico)
  (package! bibtex-actions :pin "e9128d8faf9f6e038eb84a718abba2e415e92fdf"))

(package! citeproc :pin "c8ff95862823cdff067e8cc9bb7f5ef537e8f1d9")
