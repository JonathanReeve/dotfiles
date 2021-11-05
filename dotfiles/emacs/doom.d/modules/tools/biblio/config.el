;;; tools/biblio/config.el -*- lexical-binding: t; -*-

(use-package! bibtex-completion
  :defer t
  :config
  (setq bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-pdf-field "file"));; This tell bibtex-completion to look at the File field of the bibtex to figure out which pdf to open

(use-package! ivy-bibtex
  :when (featurep! :completion ivy)
  :defer t
  :config
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus)))

(use-package! citar
  :when (featurep! :completion vertico)
  :after bibtex-completion
  :custom
  (when (featurep! +roam2)
    (citar-file-note-org-include '(org-id org-roam-ref))))

(use-package! citeproc
  :defer t)

;;; Org-Cite configuration

(use-package! oc
  :after org bibtex-completion citar
  :config
  (require 'ox)
  (map! :map org-mode-map
        :localleader
        :desc "Insert citation" "@" #'org-cite-insert)
  (setq org-cite-global-bibliography
        (let ((paths (or citar-bibliography
                         bibtex-completion-bibliography)))
          ;; Always return bibliography paths as list for org-cite.
          (if (stringp paths) (list paths) paths)))
  ;; setup export processor; default csl/citeproc-el, with biblatex for latex
  (setq org-cite-export-processors
        '((latex biblatex)
          (t csl))))

  ;;; Org-cite processors
(use-package! oc-biblatex
  :after oc)

(use-package! oc-csl
  :after oc)

(use-package! oc-natbib
  :after oc)

;;;; Third-party

(use-package! citar-org
  :no-require
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-support-shift-select t)
  (when (featurep! :lang org +roam2)
    ;; Include property drawer metadata for 'org-roam' v2.
    (citar-file-note-org-include '(org-id org-roam-ref))))
