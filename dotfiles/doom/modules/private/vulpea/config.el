;;; private/vulpea/config.el -*- lexical-binding: t; -*-

(setq org-roam-directory "~/Dokumentoj/Org/Roam"
      org-roam-dailies-directory "Daily/"
      org-roam-db-location "~/Dokumentoj/Org/Roam/org-roam.db")

(use-package! vulpea
  :hook (org-roam-db-autosync-mode . vulpea-db-autosync-enable)
  :config
  (defun my/vulpea-update-metadata ()
    "Update metadata for the current node."
    (when (vulpea-buffer-p)
      (vulpea-meta-add-tag-selection)
      (vulpea-meta-add-alias-selection)))
  (add-hook 'before-save-hook #'my/vulpea-update-metadata)

  ;; CAPF for vulpea
  (defun vulpea-capf ()
    "Completion at point function for vulpea nodes."
    (let ((annotated (vulpea-db-query)))
      (list (save-excursion (backward-word) (point))
            (point)
            (mapcar #'vulpea-node-title annotated)
            :exit-function
            (lambda (str _status)
              (let ((node (seq-find (lambda (n) (string= (vulpea-node-title n) str))
                                    annotated)))
                (when node
                  (delete-region (save-excursion (backward-word) (point)) (point))
                  (insert (format "[[id:%s][%s]]" (vulpea-node-id node) str))))))))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'vulpea-capf))))

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

(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("m" "movie" plain "** ${title}\n :PROPERTIES:\n :ID: %(org-id-uuid)\n :RATING:\n :END:\n%u\n"
           :target (file+olp "movies.org" ("watched")))
          ("b" "literature note" plain "%?" :target (file+head
                                                     "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${citar-citekey}.org"
                                                     "#+title: ${citar-citekey} (${citar-date}). ${note-title}.
#+created: %U
#+last-modified: %U

- keywords ::
- related ::

* ${note-title}
:PROPERTIES:
:Custom_ID: ${citar-citekey}
:URL: ${citar-url}
:AUTHOR: ${citar-author}
:NOTER_DOCUMENT: ${citar-file}
:NOTER_PAGE:
:END:\n"))))

  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%?" :target
           (file+head "${slug}.org" "#+title: ${title}") :unnarrowed t)
          ("m" "movie" plain "** ${title}\n :PROPERTIES:\n :ID: %(org-id-uuid)\n :RATING:\n :WIKIDATA: ${ref}\n :END:\n%u\n"
           :target (file+olp "movies.org" ("watched"))))))

(map! :leader
      (:prefix-map ("n" . "notes")
       (:prefix-map ("r" . "roam")
        :desc "Find vulpea node"   "f" #'consult-vulpea-find
        :desc "Insert vulpea node" "i" #'vulpea-insert
        :desc "Vulpea journal"     "D" #'vulpea-journal-open)))
