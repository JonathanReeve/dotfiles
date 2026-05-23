;;; private/vulpea/config.el -*- lexical-binding: t; -*-

(use-package! vulpea
  :hook (org-roam-db-autosync-mode . vulpea-db-autosync-enable)
  :config
  (defun my/vulpea-update-metadata ()
    "Update metadata for the current node."
    (when (vulpea-buffer-p)
      (vulpea-meta-add-tag-selection)
      (vulpea-meta-add-alias-selection)))
  (add-hook 'before-save-hook #'my/vulpea-update-metadata))

(use-package! vulpea-ui
  :after vulpea
  :config
  (vulpea-ui-mode 1))

(use-package! vulpea-journal
  :after (vulpea org-roam)
  :config
  (setq vulpea-journal-directory (concat org-roam-directory "/daily/")))

(use-package! consult-vulpea
  :after (vulpea consult))

(use-package! citar-vulpea
  :after (citar vulpea)
  :config (citar-vulpea-mode))

(map! :leader
      (:prefix-map ("n" . "notes")
       (:prefix-map ("r" . "roam")
        :desc "Find vulpea node"   "f" #'consult-vulpea-find
        :desc "Insert vulpea node" "i" #'vulpea-insert
        :desc "Vulpea journal"     "D" #'vulpea-journal-open)))
