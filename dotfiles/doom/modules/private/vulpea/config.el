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
                        (vulpea-ui-sidebar-open))))
  :config
  (map! :map vulpea-ui-collection-mode-map
        "RET"          #'vulpea-ui-collection-visit
        "<return>"     #'vulpea-ui-collection-visit
        :nm "RET"      #'vulpea-ui-collection-visit
        :nm "<return>" #'vulpea-ui-collection-visit
        :nm "o"        #'vulpea-ui-collection-visit-other-window
        :nm "C-o"      #'vulpea-ui-collection-preview
        :nm "v"        #'vulpea-ui-collection-switch-view
        :nm "s"        #'vulpea-ui-collection-sort
        :nm "f"        #'vulpea-ui-collection-filter-tags
        :nm "/"        #'vulpea-ui-collection-filter-search
        :nm "g"        #'vulpea-ui-collection-group-by
        :nm "r"        #'vulpea-ui-collection-refresh
        :nm "gr"       #'vulpea-ui-collection-refresh
        :nm "E"        #'vulpea-ui-collection-quick-edit
        :nm "t"        #'vulpea-ui-collection-quick-tag
        :nm "q"        #'quit-window))

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

;; ---------------------------------------------------------------------------
;; Libro — Bibliographic Layer on top of Vulpea (+ Optional Citar Adapter)
;; ---------------------------------------------------------------------------

(add-to-list 'load-path "/home/jon/Programaroj/libro")

(after! vulpea
  (require 'libro)
  (require 'libro-citar)
  (setq libro-google-books-api-key
        (lambda () (auth-source-pass-get 'secret "api.google.com/apikey")))
  (libro-citar-mode 1)
  (defalias 'my/find-paper-file #'libro-db-find-file)
  (defalias 'my/bib-enhance-buffer #'libro-enhance-buffer))

;; ---------------------------------------------------------------------------
;; Vulpea Note Creation & Template System
;; ---------------------------------------------------------------------------

(after! vulpea
  (setq vulpea-create-default-template
        '(:file-name "${timestamp}-${slug}.org")
        vulpea-find-default-create-fn #'my/vulpea-create-note-with-template
        vulpea-insert-default-note-fn #'my/vulpea-create-note-with-template))

(defun my/vulpea-format-wikidata-prop (wd-input)
  "Normalize WD-INPUT (Q-ID, Wikidata URL, or [[wd:...]]) to [[wd:Q...]]."
  (let ((trimmed (string-trim (or wd-input ""))))
    (cond
     ((string-empty-p trimmed) "")
     ((string-match "\\[\\[wd:\\(Q[0-9]+\\)\\]\\]" trimmed)
      (format "[[wd:%s]]" (match-string 1 trimmed)))
     ((string-match "\\(Q[0-9]+\\)" trimmed)
      (format "[[wd:%s]]" (match-string 1 trimmed)))
     (t trimmed))))

(defun my/vulpea-create-default (title)
  "Create a default file-level Vulpea note for TITLE."
  (let ((title (if (or (null title) (string-empty-p (string-trim title)))
                   (read-string "Title: ")
                 title)))
    (vulpea-create title "${timestamp}-${slug}.org")))

(defun my/vulpea-create-person (&optional title)
  "Create a Person Vulpea note for TITLE, prompting for Years and Wikidata Q-ID."
  (let* ((title (if (or (null title) (string-empty-p (string-trim title)))
                    (read-string "Person name: ")
                  title))
         ;; Attempt non-blocking/safe Wikidata lookup to pre-fill prompts
         (wd-details (condition-case nil
                         (when (fboundp 'my/wikidata-resolve-id)
                           (when-let* ((qid (my/wikidata-resolve-id title :person)))
                             (if (fboundp 'my/wikidata-get-person-details)
                                 (my/wikidata-get-person-details qid)
                               (list :id qid))))
                       (error nil)))
         (default-years (or (plist-get wd-details :years) ""))
         (default-qid (or (plist-get wd-details :id) ""))
         (years (read-string "Years (e.g. 1919–2021): " default-years))
         (wd-raw (read-string "Wikidata Q-ID (e.g. Q12345): " default-qid))
         (wd-prop (my/vulpea-format-wikidata-prop wd-raw))
         (props (append (unless (string-empty-p (string-trim years))
                          (list (cons "YEARS" (string-trim years))))
                        (unless (string-empty-p wd-prop)
                          (list (cons "WIKIDATA" wd-prop))))))
    (vulpea-create
     title
     "${timestamp}-${slug}.org"
     :tags '("person")
     :properties props)))

(defalias 'my/vulpea-suggest-citekey #'libro-suggest-citekey)

(defun my/vulpea-create-book (&optional title)
  "Create a Book / Literature Note in Vulpea via `libro-create'."
  (require 'libro)
  (libro-create title))

(defconst my/vulpea-project-statuses
  '("active" "stuck" "paused" "done" "archived")
  "Allowed workflow statuses for GTD / PARA / Zen-to-Done Vulpea projects.
Notes with `archived' status are excluded from `org-agenda'.")

(defconst my/vulpea-project-areas
  '("research" "writing" "teaching" "software" "career" "sangha" "home" "finance" "personal")
  "PARA Areas of Responsibility for Vulpea projects.")

(defun my/vulpea-create-project (&optional title)
  "Create a Project Vulpea note aligned with GTD, PARA, and Zen to Done."
  (interactive)
  (let* ((title (if (or (null title) (string-empty-p (string-trim title)))
                    (read-string "Project title: ")
                  title))
         (status (completing-read "Status: " my/vulpea-project-statuses nil t nil nil "active"))
         (area (completing-read "Area (PARA): " (cons "" my/vulpea-project-areas) nil nil))
         (outcome (read-string "Desired outcome (what does 'done' look like?): "))
         (priority (completing-read "Priority (A = ZTD Big Rock, B, C, or empty): "
                                    '("" "A" "B" "C") nil t))
         (props (append (list (cons "STATUS" status))
                        (unless (string-empty-p (string-trim area))
                          (list (cons "AREA" (string-trim area))))
                        (unless (string-empty-p (string-trim outcome))
                          (list (cons "OUTCOME" (string-trim outcome))))
                        (unless (string-empty-p (string-trim priority))
                          (list (cons "PRIORITY" (string-trim priority)))))))
    (setq my/vulpea-todo-files-cache nil)
    (vulpea-create
     title
     "${timestamp}-${slug}.org"
     :tags '("project")
     :properties props
     :body "* Next Actions\n** TODO \n\n* Notes\n")))

(defun my/vulpea--resolve-target-org-buffer ()
  "Return target Org buffer from current `org-mode' or `org-agenda-mode' buffer."
  (cond
   ((derived-mode-p 'org-mode) (current-buffer))
   ((derived-mode-p 'org-agenda-mode)
    (let ((marker (or (org-get-at-bol 'org-hd-marker)
                      (org-get-at-bol 'org-marker))))
      (unless (and marker (marker-buffer marker))
        (user-error "No Org entry on current agenda line"))
      (marker-buffer marker)))
   (t (user-error "Not in an org-mode or org-agenda-mode buffer"))))

(defun my/vulpea--file-drawer-get (prop)
  "Read PROP directly from the file-level `:PROPERTIES:' drawer (safe for `PRIORITY')."
  (org-with-wide-buffer
   (goto-char (point-min))
   (when (looking-at-p "[ \t]*:PROPERTIES:")
     (let ((end (save-excursion
                  (and (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                       (point))))
           (case-fold-search t))
       (when (and end (re-search-forward (format "^[ \t]*:%s:[ \t]*\\(.*\\)$" (regexp-quote prop)) end t))
         (string-trim (match-string-no-properties 1)))))))

(defun my/vulpea--file-drawer-put (prop value)
  "Set or delete PROP in the file-level `:PROPERTIES:' drawer (safe for `PRIORITY')."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((val (and value (string-trim value)))
         (case-fold-search t))
     (if (not (looking-at-p "[ \t]*:PROPERTIES:"))
         (when (and val (not (string-empty-p val)))
           (org-entry-put (point-min) prop val))
       (let ((end (save-excursion
                    (and (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                         (line-beginning-position)))))
         (when end
           (if (re-search-forward (format "^[ \t]*:%s:[ \t]*.*$" (regexp-quote prop)) end t)
               (if (and val (not (string-empty-p val)))
                   (replace-match (format ":%s: %s" (upcase prop) val) t t)
                 (delete-region (line-beginning-position)
                                (min (point-max) (1+ (line-end-position)))))
             (when (and val (not (string-empty-p val)))
               (goto-char end)
               (insert (format ":%s: %s\n" (upcase prop) val))))))))))

(defun my/vulpea-project-set-status ()
  "Interactively set `:STATUS:', `:AREA:', `:PRIORITY:', or `:OUTCOME:' on a project note.
Works both inside an `org-mode' buffer and on any project/task line in `org-agenda'."
  (interactive)
  (let ((in-agenda (derived-mode-p 'org-agenda-mode))
        (buf (my/vulpea--resolve-target-org-buffer)))
    (with-current-buffer buf
      (let* ((cur-status (or (my/vulpea--file-drawer-get "STATUS") "active"))
             (cur-area (or (my/vulpea--file-drawer-get "AREA") ""))
             (cur-outcome (or (my/vulpea--file-drawer-get "OUTCOME") ""))
             (cur-pri (or (my/vulpea--file-drawer-get "PRIORITY") ""))
             (status (completing-read "Project status: " my/vulpea-project-statuses nil t nil nil cur-status))
             (area (completing-read "Area (PARA): " (cons "" my/vulpea-project-areas) nil nil cur-area))
             (pri (completing-read "Priority (A = Big Rock, B, C, or empty): "
                                   '("" "A" "B" "C") nil t nil nil cur-pri))
             (outcome (read-string "Outcome: " cur-outcome)))
        (my/vulpea--file-drawer-put "STATUS" status)
        (my/vulpea--file-drawer-put "AREA" area)
        (my/vulpea--file-drawer-put "PRIORITY" pri)
        (my/vulpea--file-drawer-put "OUTCOME" outcome)
        (save-buffer)
        (when (buffer-file-name)
          (vulpea-db-update-file (buffer-file-name)))
        (setq my/vulpea-todo-files-cache nil)
        (message "Project updated: status=%s%s"
                 status
                 (if (string-empty-p area) "" (format " area=%s" area)))))
    (when in-agenda
      (org-agenda-redo t))))

(defun my/vulpea-project-archive ()
  "Mark the current project note (in `org-mode' or `org-agenda') as `archived'."
  (interactive)
  (let ((in-agenda (derived-mode-p 'org-agenda-mode))
        (buf (my/vulpea--resolve-target-org-buffer)))
    (with-current-buffer buf
      (let ((title (or (cadar (org-collect-keywords '("TITLE"))) (buffer-name))))
        (when (y-or-n-p (format "Archive project \"%s\" (hides from agenda)? " title))
          (org-with-wide-buffer
           (org-entry-put (point-min) "STATUS" "archived"))
          (save-buffer)
          (when (buffer-file-name)
            (vulpea-db-update-file (buffer-file-name)))
          (setq my/vulpea-todo-files-cache nil)
          (message "Archived project: %s" title))))
    (when in-agenda
      (org-agenda-redo t))))

;; ---------------------------------------------------------------------------
;; Vulpea-PARA Inspired Self-Updating Agenda & Custom Project Dashboard
;; ---------------------------------------------------------------------------
;; Re-implements the best parts of `vulpea-para' without moving files into Area
;; files:
;; 1. Fast `rg'-backed self-updating `org-agenda-files' (decoupled from `:project:'),
;;    automatically excluding notes with `:STATUS: archived' or `:ARCHIVE:'.
;; 2. Human-readable `Area > Note Title' agenda categories instead of timestamped
;;    filenames (`my/vulpea-agenda-category').
;; 3. Title-based refiling (`org-refile-use-outline-path' = `title').
;; 4. Custom Org Agenda Dashboard (`"p"' / `SPC n r a') displaying Projects with
;;    Status (`active', `stuck', `paused', `done'), Priority, Area, Outcome, and
;;    dynamic Stuck Project detection when an `active' project has 0 open TODOs.

(defvar my/vulpea-todo-files-cache nil
  "Cached `(TIMESTAMP . FILE-LIST)' for non-archived Vulpea files with open tasks.")

(defvar my/vulpea-agenda-category-cache (make-hash-table :test 'equal)
  "Short-lived cache mapping file path -> formatted base category string.")

(defun my/vulpea-archived-files-table ()
  "Return hash-table of expanded file paths whose Vulpea note has `:STATUS: archived'."
  (let ((ht (make-hash-table :test 'equal)))
    (when (fboundp 'vulpea-db-query-by-level)
      (dolist (n (vulpea-db-query-by-level 0))
        (let* ((props (vulpea-note-properties n))
               (st (cdr (assoc-string "STATUS" props t)))
               (tags (vulpea-note-tags n)))
          (when (or (and st (string-equal-ignore-case (string-trim st) "archived"))
                    (member "ARCHIVE" tags)
                    (member "archive" tags))
            (when-let ((p (vulpea-note-path n)))
              (puthash (expand-file-name p) t ht))))))
    ht))

(defun my/vulpea-todo-files ()
  "Return non-archived `.org' files in `vulpea-directory' with open TODO/WAITING headings."
  (let ((now (float-time)))
    (if (and my/vulpea-todo-files-cache
             (< (- now (car my/vulpea-todo-files-cache)) 5.0))
        (cdr my/vulpea-todo-files-cache)
      (let* ((dir (expand-file-name vulpea-directory))
             (archived-ht (my/vulpea-archived-files-table))
             (raw-files (when (and (executable-find "rg") (file-directory-p dir))
                          (process-lines "rg" "--files-with-matches" "--no-messages"
                                         "--glob" "*.org"
                                         "^\\*+ +(TODO|WAITING)\\b"
                                         dir)))
             (live-files (seq-remove (lambda (f)
                                       (gethash (expand-file-name f) archived-ht))
                                     raw-files)))
        (setq my/vulpea-todo-files-cache (cons now live-files))
        live-files))))

(defvar my/vulpea-agenda-restrict-files nil
  "When non-nil, `org-agenda-files' advice does not append general `my/vulpea-todo-files'.")

(defun my/vulpea-agenda-files-advice (files)
  "Append live Vulpea TODO files (excluding `archived' projects) to `org-agenda-files'."
  (let ((archived-ht (my/vulpea-archived-files-table)))
    (seq-remove (lambda (f) (gethash (expand-file-name f) archived-ht))
                (if my/vulpea-agenda-restrict-files
                    (seq-uniq files)
                  (seq-uniq (append files (my/vulpea-todo-files)))))))

(defun my/vulpea-agenda-category (&optional len)
  "Return human-readable `Area > Title' or `Title' category padded/truncated to LEN.
Inspired by `vulpea-para-agenda-category', adapted for file-level Vulpea notes."
  (let* ((width (or len 28))
         (file (buffer-file-name))
         (label
          (if (not file)
              (or (org-get-category) "")
            (or (gethash file my/vulpea-agenda-category-cache)
                (puthash
                 file
                 (let ((roam-dir (file-name-as-directory (expand-file-name vulpea-directory))))
                   (if (and (string-prefix-p roam-dir (expand-file-name file))
                            (fboundp 'vulpea-db-query-by-file-paths))
                       (if-let* ((note (car (vulpea-db-query-by-file-paths (list file) 0)))
                                 (title (vulpea-note-title note)))
                           (let ((area (cdr (assoc-string "AREA" (vulpea-note-properties note) t))))
                             (if (and area (not (string-empty-p (string-trim area))))
                                 (format "%s > %s" (string-trim area) title)
                               title))
                         (or (org-get-category) (file-name-base file)))
                     (or (org-get-category) (file-name-base file))))
                 my/vulpea-agenda-category-cache)))))
    (truncate-string-to-width label width nil ?\s "…")))

(defun my/vulpea--project-task-counts (paths)
  "Return hash-table mapping expanded file path -> `(TODOS . WAITING)' counts."
  (let ((ht (make-hash-table :test 'equal)))
    (when (and paths (executable-find "rg"))
      (dolist (spec `((todo . "^\\*+ +TODO\\b")
                      (wait . "^\\*+ +WAITING\\b")))
        (let ((lines (ignore-errors
                       (apply #'process-lines "rg" "--count-matches" "--no-messages"
                              "--with-filename" (cdr spec) paths))))
          (dolist (line lines)
            (when (string-match "\\`\\(.+\\):\\([0-9]+\\)\\'" line)
              (let* ((f (expand-file-name (match-string 1 line)))
                     (cnt (string-to-number (match-string 2 line)))
                     (cur (gethash f ht (cons 0 0))))
                (if (eq (car spec) 'todo)
                    (puthash f (cons cnt (cdr cur)) ht)
                  (puthash f (cons (car cur) cnt) ht))))))))
    ht))

(defun my/vulpea--project-status-rank (effective-status)
  "Sort order for EFFECTIVE-STATUS in the Projects agenda block."
  (pcase effective-status
    ("active" 1)
    ("stuck"  2)
    ("paused" 3)
    ("done"   4)
    (_        5)))

(defun my/vulpea--project-priority-rank (pri)
  "Sort order for project priority string PRI (`A' < `B' < `C' < empty)."
  (if (and pri (not (string-empty-p pri)))
      (- (upcase (aref pri 0)) ?A)
    99))

(defun my/vulpea-agenda-projects-block (&optional match)
  "Custom `org-agenda' block listing non-archived Vulpea `#project' notes.
If MATCH is \"active\", only `active' and `stuck' projects are shown; otherwise
all non-archived projects (`active', `stuck', `paused', `done') are shown.
Active projects with 0 open TODO/WAITING headings are dynamically flagged as `STUCK*'."
  (unless org-agenda-multi
    (org-agenda-prepare "Projects"))
  (clrhash my/vulpea-agenda-category-cache)
  (let* ((inhibit-read-only t)
         (all-projects (when (fboundp 'vulpea-db-query-by-tags-some)
                         (seq-filter
                          (lambda (n)
                            (and (= (vulpea-note-level n) 0)
                                 (or (not (fboundp 'vulpea-schema-applies-p))
                                     (vulpea-schema-applies-p n 'project))))
                          (vulpea-db-query-by-tags-some '("project")))))
         (live-projects
          (seq-filter
           (lambda (n)
             (let ((st (downcase (or (cdr (assoc-string "STATUS" (vulpea-note-properties n) t))
                                     "active"))))
               (and (not (string= st "archived"))
                    (if (and (stringp match) (string= match "active"))
                        (member st '("active" "stuck"))
                      t))))
           all-projects))
         (paths (delq nil (mapcar (lambda (n)
                                    (and (vulpea-note-path n)
                                         (expand-file-name (vulpea-note-path n))))
                                  live-projects)))
         (counts-ht (my/vulpea--project-task-counts paths))
         (rows
          (mapcar
           (lambda (n)
             (let* ((path (expand-file-name (vulpea-note-path n)))
                    (props (vulpea-note-properties n))
                    (raw-st (downcase (or (cdr (assoc-string "STATUS" props t)) "active")))
                    (pri (let ((p (cdr (assoc-string "PRIORITY" props t))))
                           (and p (not (string-empty-p (string-trim p))) (upcase (string-trim p)))))
                    (area (or (cdr (assoc-string "AREA" props t)) ""))
                    (outcome (or (cdr (assoc-string "OUTCOME" props t)) ""))
                    (cnts (gethash path counts-ht (cons 0 0)))
                    (todos (car cnts))
                    (waits (cdr cnts))
                    (auto-stuck (and (string= raw-st "active") (= todos 0) (= waits 0)))
                    (eff-st (if auto-stuck "stuck" raw-st)))
               (list :note n :path path :raw-st raw-st :eff-st eff-st :auto-stuck auto-stuck
                     :pri pri :area area :outcome outcome :todos todos :waits waits)))
           live-projects))
         (sorted
          (sort rows
                (lambda (a b)
                  (let ((ra (my/vulpea--project-status-rank (plist-get a :eff-st)))
                        (rb (my/vulpea--project-status-rank (plist-get b :eff-st))))
                    (if (/= ra rb)
                        (< ra rb)
                      (let ((pa (my/vulpea--project-priority-rank (plist-get a :pri)))
                            (pb (my/vulpea--project-priority-rank (plist-get b :pri))))
                        (if (/= pa pb)
                            (< pa pb)
                          (string-lessp (vulpea-note-title (plist-get a :note))
                                        (vulpea-note-title (plist-get b :note)))))))))))
    (goto-char (point-max))
    (when (and (not (bobp)) (not org-agenda-compact-blocks) org-agenda-block-separator)
      (insert (if (characterp org-agenda-block-separator)
                  (concat (make-string (max 40 (window-width)) org-agenda-block-separator) "\n")
                (concat org-agenda-block-separator "\n"))))
    (insert (propertize (concat (or org-agenda-overriding-header
                                    "Projects (active / stuck / paused / done — archived hidden)")
                                "\n")
                        'face 'org-agenda-structure))
    (if (null sorted)
        (insert (propertize "  (No matching projects)\n" 'face 'shadow))
      (dolist (row sorted)
        (let* ((note (plist-get row :note))
               (path (plist-get row :path))
               (eff-st (plist-get row :eff-st))
               (auto-stuck (plist-get row :auto-stuck))
               (pri (plist-get row :pri))
               (area (plist-get row :area))
               (outcome (plist-get row :outcome))
               (todos (plist-get row :todos))
               (waits (plist-get row :waits))
               (buf (and (file-exists-p path) (org-get-agenda-file-buffer path)))
               (marker (when buf
                         (with-current-buffer buf
                           (org-agenda-new-marker (point-min)))))
               (st-label (pcase eff-st
                           ("active" (propertize "ACTIVE " 'face 'org-todo))
                           ("stuck"  (propertize (if auto-stuck "STUCK* " "STUCK  ")
                                                 'face 'org-warning))
                           ("paused" (propertize "PAUSED " 'face 'shadow))
                           ("done"   (propertize "DONE   " 'face 'org-done))
                           (_        (format "%-7s" (upcase eff-st)))))
               (pri-str (if pri
                            (propertize (format "[#%s]" pri) 'face 'org-priority)
                          "    "))
               (area-str (propertize (truncate-string-to-width
                                      (if (string-empty-p area) "-" area) 10 nil ?\s "…")
                                     'face 'org-tag))
               (title-str (truncate-string-to-width
                           (vulpea-note-title note) 36 nil ?\s "…"))
               (task-str (cond
                          ((and (> todos 0) (> waits 0))
                           (format "%2d TODO, %d WAIT" todos waits))
                          ((> todos 0)
                           (format "%2d TODO        " todos))
                          ((> waits 0)
                           (format " 0 TODO, %d WAIT" waits))
                          ((string= eff-st "stuck")
                           (propertize " 0 TODO (define next!)" 'face 'org-warning))
                          (t " 0 TODO        ")))
               (out-str (if (string-empty-p outcome)
                            ""
                          (propertize (concat "  — " (truncate-string-to-width outcome 42 nil nil "…"))
                                      'face 'shadow)))
               (line (format "  %s %s  %s  %s  %s%s"
                             pri-str st-label area-str title-str task-str out-str))
               (bol (point)))
          (insert line "\n")
          (add-text-properties
           bol (1- (point))
           (list 'org-marker marker
                 'org-hd-marker marker
                 'org-agenda-type 'tags
                 'org-category (if (string-empty-p area) "project" area)
                 'todo-state (upcase eff-st)
                 'priority (if pri (- ?Z (aref pri 0)) 0)
                 'undone-face 'default
                 'done-face 'org-agenda-done
                 'mouse-face 'highlight
                 'help-echo (format "RET: open %s | s: set project status | A: archive project"
                                    (file-name-nondirectory path)))))))
    (unless org-agenda-multi
      (goto-char (point-min))
      (org-agenda-fit-window-to-buffer)
      (org-agenda-finalize))))

(defun my/vulpea-agenda-active-project-files ()
  "Return agenda files belonging to `active' or `stuck' Vulpea projects (plus `Projects/')."
  (let ((proj-dirs (list (expand-file-name "~/Dokumentoj/Org/Projects/")))
        (active-paths nil))
    (when (fboundp 'vulpea-db-query-by-tags-some)
      (dolist (n (vulpea-db-query-by-tags-some '("project")))
        (when (= (vulpea-note-level n) 0)
          (let ((st (downcase (or (cdr (assoc-string "STATUS" (vulpea-note-properties n) t))
                                  "active")))
                (p (vulpea-note-path n)))
            (when (and p (member st '("active" "stuck")) (file-exists-p p))
              (push (expand-file-name p) active-paths))))))
    (seq-uniq (append proj-dirs (nreverse active-paths)))))

(defun my/vulpea-agenda-skip-project-notes ()
  "Skip entry if current buffer is a `#project' Vulpea note (used for standalone tasks block)."
  (when-let* ((file (buffer-file-name))
              (note (and (fboundp 'vulpea-db-query-by-file-paths)
                         (car (vulpea-db-query-by-file-paths (list file) 0)))))
    (when (or (member "project" (vulpea-note-tags note))
              (string-prefix-p (expand-file-name "~/Dokumentoj/Org/Projects/")
                               (expand-file-name file)))
      (point-max))))

(defun my/vulpea-projects-agenda ()
  "Open the `vulpea-para'-inspired Projects & PARA Agenda Dashboard."
  (interactive)
  (org-agenda nil "p"))

(after! org
  (advice-add 'org-agenda-files :filter-return #'my/vulpea-agenda-files-advice)
  ;; Show human-readable document titles instead of timestamped slugs when refiling
  (setq org-refile-use-outline-path 'title))

(after! org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (clrhash my/vulpea-agenda-category-cache)))
  (setq org-agenda-prefix-format
        '((agenda . " %i %(my/vulpea-agenda-category 28) %?-12t% s")
          (todo   . " %i %(my/vulpea-agenda-category 28) ")
          (tags   . " %i %(my/vulpea-agenda-category 28) ")
          (search . " %i %(my/vulpea-agenda-category 28) ")))
  ;; Quick project status & archive keys directly inside org-agenda
  (map! :map org-agenda-mode-map
        "C-c C-s" #'org-agenda-schedule)
  (after! evil-org-agenda
    (map! :map evil-org-agenda-mode-map
          :m "S" #'my/vulpea-project-set-status
          :m "A" #'my/vulpea-project-archive))
  ;; Custom `vulpea-para'-inspired Agenda Views
  (dolist (cmd
           '(("p" "PARA Projects & Actions Dashboard"
              ((my/vulpea-agenda-projects-block ""
                ((org-agenda-overriding-header "Projects (Active / Stuck / Paused / Done — Archived Hidden)")))
               (agenda ""
                ((org-agenda-span 'day)
                 (org-agenda-start-on-weekday nil)
                 (org-agenda-overriding-header "Today")))
               (todo "TODO"
                ((org-agenda-overriding-header "Active & Stuck Project Next Actions")
                 (my/vulpea-agenda-restrict-files t)
                 (org-agenda-files (my/vulpea-agenda-active-project-files))
                 (org-agenda-todo-ignore-scheduled 'future)))
               (todo "WAITING"
                ((org-agenda-overriding-header "Waiting / Blocked")))
               (todo "TODO"
                ((org-agenda-overriding-header "Standalone Tasks in Literature, Area & Concept Notes")
                 (org-agenda-skip-function #'my/vulpea-agenda-skip-project-notes)
                 (org-agenda-todo-ignore-scheduled 'future)))))
             ("P" "Active & Stuck Projects Focus"
              ((my/vulpea-agenda-projects-block "active"
                ((org-agenda-overriding-header "Focus Projects (Active & Stuck Only)")))
               (todo "TODO"
                ((org-agenda-overriding-header "Next Actions for Active Projects")
                 (my/vulpea-agenda-restrict-files t)
                 (org-agenda-files (my/vulpea-agenda-active-project-files))
                 (org-agenda-todo-ignore-scheduled 'future)))))))
    (setf (alist-get (car cmd) org-agenda-custom-commands nil nil #'equal)
          (cdr cmd))))

(defun my/vulpea-create-media (title media-type)
  "Create a Movie (:movie) or TV Show (:tv) heading note for TITLE and return it."
  (let* ((title (if (or (null title) (string-empty-p (string-trim title)))
                    (read-string (if (eq media-type :tv) "TV Show Title: " "Movie Title: "))
                  title))
         (target-file (expand-file-name
                       (if (eq media-type :tv) "20220427210214-tv.org" "movies.org")
                       vulpea-directory))
         (target-heading (if (eq media-type :tv) "Watched" "watched"))
         (entity-id (condition-case nil
                        (when (fboundp 'my/wikidata-resolve-id)
                          (my/wikidata-resolve-id title media-type))
                      (error nil)))
         (details (when (and entity-id (fboundp 'my/wikidata-get-details))
                    (condition-case nil
                        (my/wikidata-get-details entity-id)
                      (error nil))))
         (year (read-string "Year: " (or (plist-get details :year) "")))
         (wd-raw (read-string "Wikidata Q-ID: " (or entity-id "")))
         (wd-prop (my/vulpea-format-wikidata-prop wd-raw))
         (full-title (if (and (not (string-empty-p year))
                              (not (string-match-p "([12][0-9]\\{3\\})" title)))
                         (format "%s (%s)" title year)
                       title))
         (id (org-id-uuid))
         (today (if (fboundp 'my/format-eo-date)
                    (my/format-eo-date)
                  (format-time-string "[%Y-%m-%d %a]"))))
    (with-current-buffer (find-file-noselect target-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (if (re-search-forward (format "^\\* +%s\\b" (regexp-quote target-heading)) nil t)
           (forward-line 1)
         (goto-char (point-max))
         (insert (format "\n* %s\n" target-heading)))
       (insert (format "** %s\n:PROPERTIES:\n:ID:       %s\n%s%s:RATING:   ***\n:END:\n%s\n\n"
                       full-title
                       id
                       (if (string-empty-p year) "" (format ":YEAR:     %s\n" year))
                       (if (string-empty-p wd-prop) "" (format ":WIKIDATA: %s\n" wd-prop))
                       today)))
      (save-buffer))
    (org-id-add-location id target-file)
    (vulpea-db-update-file target-file)
    (vulpea-db-get-by-id id)))

(defun my/vulpea-select-template (&optional title)
  "Prompt user to select a Vulpea note template for TITLE."
  (let ((choice (read-char-choice
                 (format "Template%s: [d]efault  [p]erson  [P]roject  [r/b]ook  [m]ovie  [T]v: "
                         (if title (format " for \"%s\"" title) ""))
                 '(?d ?p ?P ?r ?b ?m ?T))))
    (pcase choice
      (?d "d")
      (?p "p")
      (?P "P")
      ((or ?r ?b) "r")
      (?m "m")
      (?T "T"))))

(defun my/vulpea-create-note-with-template (title &optional _props)
  "Prompt for a template and create a new Vulpea note with TITLE.
Used as `vulpea-insert-default-note-fn' and `vulpea-find-default-create-fn'."
  (let ((key (my/vulpea-select-template title)))
    (pcase key
      ("d" (my/vulpea-create-default title))
      ("p" (my/vulpea-create-person title))
      ("P" (my/vulpea-create-project title))
      ("r" (my/vulpea-create-book title))
      ("m" (my/vulpea-create-media title :movie))
      ("T" (my/vulpea-create-media title :tv))
      (_   (my/vulpea-create-default title)))))

(defun vulpea-capture (&optional template-key title)
  "Create and visit a Vulpea note using TEMPLATE-KEY (\"d\", \"p\", \"P\", \"r\", \"m\", \"T\")."
  (interactive)
  (let* ((key (or template-key (my/vulpea-select-template title)))
         (note (pcase key
                 ("d" (my/vulpea-create-default title))
                 ("p" (my/vulpea-create-person title))
                 ("P" (my/vulpea-create-project title))
                 ((or "r" "b") (my/vulpea-create-book title))
                 ("m" (my/vulpea-create-media title :movie))
                 ("T" (my/vulpea-create-media title :tv))
                 (_   (my/vulpea-create-default title)))))
    (when note
      (vulpea-visit note))
    note))

(defun my/org-capture-vulpea-book ()
  "Target function for `org-capture' template \"b\" (Book / Literature Note)."
  (let* ((initial-title (when (and (bound-and-true-p org-capture-plist)
                                   (plist-get org-capture-plist :initial))
                          (let ((init (plist-get org-capture-plist :initial)))
                            (unless (string-empty-p (string-trim init))
                              (string-trim init)))))
         (note (my/vulpea-create-book initial-title)))
    (unless note
      (user-error "Failed to create Book (Literature Note)"))
    (set-buffer (find-file-noselect (vulpea-note-path note)))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))))

(defun my/org-capture-vulpea-person ()
  "Target function for `org-capture' template \"p\" (Person)."
  (let* ((initial-title (when (and (bound-and-true-p org-capture-plist)
                                   (plist-get org-capture-plist :initial))
                          (let ((init (plist-get org-capture-plist :initial)))
                            (unless (string-empty-p (string-trim init))
                              (string-trim init)))))
         (note (my/vulpea-create-person initial-title)))
    (unless note
      (user-error "Failed to create Person note"))
    (set-buffer (find-file-noselect (vulpea-note-path note)))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))))

(defun my/org-capture-vulpea-project ()
  "Target function for `org-capture' template \"P\" (Project)."
  (let* ((initial-title (when (and (bound-and-true-p org-capture-plist)
                                   (plist-get org-capture-plist :initial))
                          (let ((init (plist-get org-capture-plist :initial)))
                            (unless (string-empty-p (string-trim init))
                              (string-trim init)))))
         (note (my/vulpea-create-project initial-title)))
    (unless note
      (user-error "Failed to create Project note"))
    (set-buffer (find-file-noselect (vulpea-note-path note)))
    (goto-char (point-min))
    (when (re-search-forward "^\\*\\* TODO " nil t)
      (goto-char (line-end-position)))))

  ;; Bindings - kept at top level to ensure the prefix map is always defined
  (map! :leader
        (:prefix-map ("n" . "notes")
         (:prefix-map ("r" . "roam")
          :desc "Find vulpea node"   "f" #'vulpea-find
          :desc "Find libro entry"   "b" #'libro-find
          :desc "Create libro entry" "B" #'libro-create
          :desc "Vulpea grep"        "g" #'consult-vulpea-grep
          :desc "Insert vulpea node" "i" #'vulpea-insert
          :desc "Vulpea journal"     "D" #'vulpea-journal
          :desc "Enhance metadata"   "e" #'libro-enhance-buffer
          :desc "Collection views"   "c" #'vulpea-ui-collection
          :desc "Schema dashboard"   "s" #'vulpea-ui-schema-dashboard
          :desc "PARA projects agenda" "a" #'my/vulpea-projects-agenda
          :desc "Create project"     "P" #'my/vulpea-create-project
          :desc "Set project status" "p" #'my/vulpea-project-set-status
          :desc "Archive project"    "A" #'my/vulpea-project-archive)
         (:prefix-map ("b" . "libro")
          :desc "Find libro entry"     "f" #'libro-find
          :desc "Create libro entry"   "c" #'libro-create
          :desc "Capture from URL"     "u" #'libro-capture-url
          :desc "Enhance note buffer"  "e" #'libro-enhance-buffer
          :desc "Import BibTeX"        "i" #'libro-import-bibtex
          :desc "Open PDF/EPUB file"   "o" #'libro-open-file
          :desc "Attach recent file"   "a" #'libro-attach-recent-file
          :desc "Set reading status"   "s" #'libro-set-status
          :desc "Rate work"            "r" #'libro-rate
          :desc "Link authors/editors" "p" #'libro-link-authors
          :desc "Validate schema"      "v" #'libro-schema-check-buffer
          :desc "Schema health report" "h" #'libro-schema-health
          :desc "Export BibTeX"        "x" #'libro-export-bib)))

(defun my/vulpea-bib-url-p (url)
  "Return non-nil if URL points to a known book/article metadata source."
  (and (stringp url)
       (or (string-match-p "books\\.google\\.[a-z.]+" url)
           (string-match-p "goodreads\\.com/book/show/" url)
           (string-match-p "openlibrary\\.org/\\(?:works\\|books\\|isbn\\)/" url)
           (string-match-p "https?://\\(?:dx\\.\\)?doi\\.org/10\\." url))))

(defun my/vulpea-capture-url (url title &optional selected)
  "Create a Vulpea note for URL with TITLE and optional SELECTED text.
Automatically delegates to `libro-capture-url' for Google Books, Goodreads,
OpenLibrary, and DOI URLs."
  (interactive "sURL: \nsTitle: \nsSelected: ")
  (require 'vulpea)
  (require 'libro)
  (if (my/vulpea-bib-url-p url)
      (libro-capture-url url title selected)
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
        (select-frame-set-input-focus (selected-frame))))))

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

(load! "schemas.el")


