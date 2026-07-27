;;; private/vulpea/config.el -*- lexical-binding: t; -*-

(setq vulpea-directory "~/Dokumentoj/Org/Roam"
      vulpea-db-sync-directories '("~/Dokumentoj/Org/Roam" "~/Dokumentoj/Org")
      vulpea-db-location "~/Dokumentoj/Org/Roam/vulpea.db")

(use-package! vulpea
  :config
  (vulpea-db-autosync-mode 1)
  (setq vulpea-select-describe-fn #'vulpea-select-describe-outline-full)
  (after! consult-vulpea
    (consult-vulpea-mode 1))
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
            (mapcar #'vulpea-note-title annotated)
            :exit-function
            (lambda (str _status)
              (let ((node (seq-find (lambda (n) (string= (vulpea-note-title n) str))
                                    annotated)))
                (when node
                  (delete-region (save-excursion (backward-word) (point)) (point))
                  (insert (format "[[id:%s][%s]]" (vulpea-note-id node) str))))))))

  ;; (add-hook 'org-mode-hook
  ;;           (lambda ()
  ;;             (add-to-list 'completion-at-point-functions #'vulpea-capf)))
  )

(use-package! vulpea-ui
  :after vulpea
  :hook (org-mode . (lambda () 
                      (when (and (fboundp 'vulpea-buffer-p)
                                 (buffer-file-name) 
                                 (vulpea-buffer-p)) 
                        (vulpea-ui-sidebar-open)))))

(use-package! vulpea-journal
  :after vulpea
  :config
  (vulpea-journal-setup)
  (setq! vulpea-journal-default-template
         '(:file-name "Daily/%Y-%m-%d.org"
           :title "%Y-%m-%d"
           :tags ("journal")
           :properties (("DRINKS" . "")
                        ("PHONE" . "")
                        ("EXERCISE" . "")
                        ("MOOD" . ""))
           :head "#+created: %<[%Y-%m-%d]>

#+BEGIN: clocktable :scope agenda :maxlevel 2 :step day :fileskip0 true :tstart \"%<%Y-%m-%d>\" :tend \"%(my/tomorrow)\"
#+END: ")))

(use-package! consult-vulpea
  :after (vulpea consult)
  :config
  (consult-vulpea-mode 1))

(use-package! citar-vulpea
  :after (citar vulpea)
  :config 
  (citar-vulpea-mode)

  (defun my/find-paper-file (citekey)
    "Find a PDF or EPUB in papers directories matching CITEKEY.
Returns the absolute path if found, otherwise nil."
    (let* ((search-dirs '("~/Dokumentoj/Papers/" "~/Dokumentoj/Org/Roam/shared/papers/"))
           (found nil))
      (while (and search-dirs (not found))
        (let* ((dir (expand-file-name (car search-dirs)))
               (files (when (file-directory-p dir)
                        (directory-files dir t (regexp-quote citekey)))))
          (setq found (seq-find (lambda (f) (string-match-p "\\.\\(pdf\\|epub\\)$" f))
                                files))
          (setq search-dirs (cdr search-dirs))))
      found)))

;; LITERATURE NOTE OVERRIDE
;; We override this globally to ensure citar-vulpea uses our template
(after! citar-vulpea
  (require 'vulpea)
  ;; Ensure we use :REFERENCES: instead of :ROAM_REFS:
  (setq citar-vulpea-references-property "REFERENCES")
  
  (defun citar-vulpea--create-note (citekey &optional _entry)
    "Create a new bibliographic note for CITEKEY.
This override ensures the literature note template is applied and avoids overwriting."
    (let* ((filename (concat citekey ".org"))
           (path (expand-file-name filename vulpea-directory))
           ;; Use :REFERENCES: with cite: prefix
           (ref (concat "cite:" citekey))
           (node (or (car (vulpea-db-query-by-property "REFERENCES" ref))
                    (car (vulpea-db-query-by-property "ROAM_REFS" ref)) ;; Fallback for migration
                    (seq-find (lambda (n) (string= (vulpea-note-path n) path))
                              (vulpea-db-query)))))
      (cond
       ;; 1. Node exists in DB -> visit it
       (node
        (vulpea-visit node)
        path)
       ;; 2. File exists on disk but not in DB -> open it
       ((file-exists-p path)
        (find-file path)
        path)
       ;; 3. Truly new note -> create it
       (t
        (let* ((entry (citar-get-entry citekey))
               (title (or (citar-vulpea--format-note-title citekey)
                          (read-string "Title: ")))
               (bib-file (citar-get-value "file" entry))
               (paper-file (my/find-paper-file citekey))
               (author (or (citar-get-value "author" entry) ""))
               (keywords (or (citar-get-value "keywords" entry) ""))
               (url (or (citar-get-value "url" entry) ""))
               (noter-doc (or bib-file paper-file ""))
               (note (vulpea-create
                      title
                      filename
                      :tags (list citar-vulpea-keyword)
                      :properties `(("REFERENCES" . ,ref)
                                    ("CUSTOM_ID" . ,citekey)
                                    ("AUTHOR" . ,author)
                                    ("URL" . ,url)
                                    ("NOTER_DOCUMENT" . ,noter-doc)
                                    ("NOTER_PAGE" . ""))
                      :head (format "#+created: %s
#+last-modified: %s

- keywords :: %s
- related ::

* %s
"
                                    (format-time-string "[%Y-%m-%d %a %H:%M]")
                                    (format-time-string "[%Y-%m-%d %a %H:%M]")
                                    keywords
                                    title))))
          (when note
            (vulpea-visit note)
            (vulpea-note-path note))))))))

(setq vulpea-capture-templates
      '(("d" "default" plain "%?" :target
         (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :target (file+head "${citekey}.org"
                            "#+title: ${title}
:PROPERTIES:
:CUSTOM_ID: ${citekey}
:AUTHOR: ${author}
:URL: ${url}
:NOTER_DOCUMENT: ${noter-document}
:NOTER_PAGE:
:END:
#+filetags: :${tags}:
#+created: %u
#+last-modified: %u

- keywords :: ${keywords}
- related ::

* ${title}
")
         :unnarrowed t)
        ("m" "movie" plain "** ${title}\n :PROPERTIES:\n :ID: %(org-id-uuid)\n :RATING:\n :END:\n%u\n"
         :target (file+olp "movies.org" ("watched")))))

  ;; Bindings - kept at top level to ensure the prefix map is always defined
  (map! :leader
        (:prefix-map ("n" . "notes")
         (:prefix-map ("r" . "roam")
          :desc "Find vulpea node"   "f" #'vulpea-find
          :desc "Vulpea grep"        "g" #'consult-vulpea-grep
          :desc "Insert vulpea node" "i" #'vulpea-insert
          :desc "Vulpea journal"     "D" #'vulpea-journal
          :desc "Enhance metadata"   "e" #'my/bib-enhance-buffer)))

(defun my/vulpea-capture-url (url title &optional selected)
  "Create a Vulpea note for URL with TITLE and optional SELECTED text."
  (interactive "sURL: \nsTitle: \nsSelected: ")
  (require 'vulpea)
  (let* ((body (if (or (null selected) (string-empty-p selected))
                   ""
                 (concat "#+begin_quote\n" selected "\n#+end_quote")))
         (note (vulpea-create
                title
                nil
                :properties `(("REFERENCES" . ,url))
                :body body)))
    (when note
      (vulpea-visit note)
      (select-frame-set-input-focus (selected-frame)))))

(defun my/org-protocol-vulpea-capture (data)
  "Process `org-protocol://vulpea-capture` URL with DATA plist."
  (let ((url (plist-get data :url))
        (title (or (plist-get data :title) "Bookmark"))
        (body (plist-get data :body)))
    (my/vulpea-capture-url url title body)
    nil))

(after! org-protocol
  (add-to-list 'org-protocol-protocol-alist
               '("vulpea-capture"
                 :protocol "vulpea-capture"
                 :function my/org-protocol-vulpea-capture
                 :kill-client t)))

(load! "/home/jon/Agordoj/scripts/enhance-bib-metadata.el")


