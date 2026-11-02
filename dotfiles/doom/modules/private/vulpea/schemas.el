;;; private/vulpea/schemas.el --- Note schemas and validation -*- lexical-binding: t; -*-

;; Declarative schemas for Vulpea notes.
;; Provides validation and collection health checking for:
;; - Books (org-bibtex specification)
;; - People (DOB/DOD years and wikidata)
;; - Movies (year and wikidata)
;; - TV shows (first year aired and wikidata)

(require 'cl-lib)
(require 'vulpea)
(require 'vulpea-schema)
(add-to-list 'load-path "/home/jon/Programaroj/libro")
(require 'libro-schema)

;;; ----------------------------------------------------------------------------
;;; Property & Keyword Bridge (Delegated to `libro-schema')
;;; ----------------------------------------------------------------------------
;; `libro-schema' installs the unified property/keyword/meta bridge on
;; `vulpea-note-meta-get-list', `vulpea--schema-buffer-note', and
;; `vulpea-schema-fix-violation', and defines all 12 `ol-bibtex' bibliographic
;; schemas (`book', `article', `incollection', `inbook', `inproceedings',
;; `phdthesis', `mastersthesis', `techreport', `booklet', `proceedings',
;; `unpublished', `misc').

(defalias 'my/vulpea-get-file-keyword #'libro-schema--get-file-keyword)
(defalias 'my/vulpea-note-raw-prop #'libro-schema-note-raw-prop)

(defun my/vulpea-ui-collection-note-property-unified (orig-fn note key)
  "Around advice on `vulpea-ui-collection--note-property' to fall back to unified properties."
  (or (funcall orig-fn note key)
      (when (string-equal-ignore-case key "VENUE")
        (let ((props (vulpea-note-properties note)))
          (or (cdr (assoc-string "JOURNAL" props t))
              (cdr (assoc-string "JOURNALTITLE" props t))
              (cdr (assoc-string "BOOKTITLE" props t))
              (cdr (assoc-string "PUBLISHER" props t))
              (cdr (assoc-string "SCHOOL" props t))
              (cdr (assoc-string "INSTITUTION" props t)))))
      (libro-schema-note-raw-prop note key)))

(with-eval-after-load 'vulpea-ui
  (advice-add 'vulpea-ui-collection--note-property
              :around #'my/vulpea-ui-collection-note-property-unified))

(defun my/vulpea-schema-fix-drawer-advice (orig-fn violation &optional bound)
  "Write fixed VIOLATION value to `:PROPERTIES:' for `project', `area', and `person' schemas."
  (let ((sname (let ((s (vulpea-violation-schema violation)))
                 (if (vulpea-schema-p s) (vulpea-schema-name s) s))))
    (if (not (memq sname '(project area person)))
        (funcall orig-fn violation bound)
      (let* ((schema (vulpea-schema--resolve sname))
             (field (cl-find (vulpea-violation-field violation)
                             (vulpea-schema-fields schema)
                             :key (lambda (f) (plist-get f :key))
                             :test #'equal))
             (note (vulpea--schema-buffer-note schema))
             (required (vulpea-schema--call-or-value (plist-get field :required) note))
             (value (vulpea--schema-prompt-field
                     field note required
                     (vulpea--schema-field-default field note)))
             (value (if (listp value) (remove "" value) value)))
        (when (and field value (not (equal value "")))
          (if (and (string-equal-ignore-case (plist-get field :key) "priority")
                   (fboundp 'my/vulpea--file-drawer-put))
              (my/vulpea--file-drawer-put "PRIORITY" (format "%s" value))
            (libro-schema--put-buffer-property (plist-get field :key) value (or bound 'buffer)))
          value)))))

(advice-add 'vulpea-schema-fix-violation :around #'my/vulpea-schema-fix-drawer-advice)

;;; ----------------------------------------------------------------------------
;;; Schema Definitions
;;; ----------------------------------------------------------------------------

;; Abstract Media Work Schema (Composition base for Movie and TV)
(vulpea-schema-define 'media-work
  :fields
  '((:key "wikidata" :type string :required t
          :validate (lambda (val _n)
                      (if (or (string-match-p "wd:Q[0-9]+" val)
                              (string-match-p "Q[0-9]+" val))
                          t
                        "Wikidata must be a valid Q-identifier (e.g. [[wd:Q12345]] or wd:Q12345)")))
    (:key "year" :type number :required t)
    (:key "rating" :type string)))

;; Movie Schema
(vulpea-schema-define 'movie
  :predicate (lambda (n)
               (and (> (vulpea-note-level n) 1)
                    (or (member "movie" (vulpea-note-tags n))
                        (string-suffix-p "movies.org" (vulpea-note-path n)))))
  :include 'media-work
  :fields
  '((:key "director" :type note :target-tags ("person"))))

;; TV Show Schema
(vulpea-schema-define 'tv
  :predicate (lambda (n)
               (and (> (vulpea-note-level n) 1)
                    (or (member "tv" (vulpea-note-tags n))
                        (string-suffix-p "20220427210214-tv.org" (vulpea-note-path n)))))
  :include 'media-work)

;; Person Schema
(vulpea-schema-define 'person
  :predicate (lambda (n)
               (and (= (vulpea-note-level n) 0)
                    (let ((tags (vulpea-note-tags n)))
                      (and (not (libro-schema-bib-note-p n))
                           (not (member "concept" tags))
                           (not (member "periodical" tags))
                           (not (member "project" tags))
                           (not (member "area" tags))
                           (not (member "journal" tags))
                           (or (member "person" tags)
                               (member "author" tags)
                               (my/vulpea-note-raw-prop n "years"))))))
  :fields
  '((:key "years" :type string
          :required (lambda (n)
                      (not (member "contemporary" (vulpea-note-tags n)))))
    (:key "wikidata" :type string)))

;; Project Schema (GTD + PARA + Zen to Done)
;; - PARA: Finite effort with a finish line, optionally linked to an `:AREA:'.
;;         `archived' projects are excluded from `org-agenda'.
;; - GTD: Clear `:STATUS:' (`active', `stuck', `paused', `done', `archived')
;;        and `:OUTCOME:' (definition of "done").
;; - Zen to Done: Limit `active' projects to 3–5 "Big Rocks" (`:PRIORITY: A').
(vulpea-schema-define 'project
  :predicate (lambda (n)
               (and (= (vulpea-note-level n) 0)
                    (let ((tags (vulpea-note-tags n)))
                      (and (member "project" tags)
                           (not (libro-schema-bib-note-p n))
                           (not (member "journal" tags))))))
  :fields
  '((:key "status" :type symbol :required t
          :one-of (active stuck paused done archived)
          :default "active")
    (:key "area" :type symbol
          :one-of (research writing teaching software career sangha home finance personal))
    (:key "outcome" :type string)
    (:key "priority" :type symbol :one-of (a b c d e f g A B C D E F G))
    (:key "deadline" :type date)))

;; PARA Area of Responsibility Schema (ongoing sphere without an end date)
(vulpea-schema-define 'area
  :predicate (lambda (n)
               (and (= (vulpea-note-level n) 0)
                    (member "area" (vulpea-note-tags n))))
  :fields
  '((:key "title" :type string :required t)))

;; Note: All bibliographic schemas (`book', `article', `incollection', `inbook',
;; `inproceedings', `phdthesis', `mastersthesis', `techreport', `booklet',
;; `proceedings', `unpublished', `misc') are defined by `libro-schema'.

;; Concept / Topic Schema
(vulpea-schema-define 'concept
  :predicate (lambda (n)
               (and (= (vulpea-note-level n) 0)
                    (let ((props (vulpea-note-properties n))
                          (tags (vulpea-note-tags n))
                          (path (vulpea-note-path n)))
                      (and (not (libro-schema-bib-note-p n))
                           (not (vulpea-schema-applies-p n 'person))
                           (not (vulpea-schema-applies-p n 'project))
                           (not (vulpea-schema-applies-p n 'area))
                           (or (member "concept" tags)
                               (member "periodical" tags)
                               (and (string-prefix-p (expand-file-name "~/Dokumentoj/Org/Roam/") path)
                                    (not (string-match-p "/Daily/" path))
                                    (not (string-suffix-p "movies.org" path))
                                    (not (string-suffix-p "20220427210214-tv.org" path))
                                    (not (member "journal" tags))
                                    (not (assoc-string "REFERENCES" props t))
                                    (not (assoc-string "CUSTOM_ID" props t))
                                    (not (assoc-string "AUTHOR" props t))))))))
  :fields
  '((:key "title" :type string :required t)
    (:key "wikidata" :type string)))

;;; ----------------------------------------------------------------------------
;;; Collection Views (vulpea-ui-collection-views)
;;; ----------------------------------------------------------------------------

(defun my/vulpea-bookmark-note-p (n)
  "Return non-nil if N is a web bookmark / link note (not a bibliographic note)."
  (and (= (vulpea-note-level n) 0)
       (not (libro-schema-bib-note-p n))
       (not (vulpea-schema-applies-p n 'person))
       (let ((props (vulpea-note-properties n)))
         (and (assoc-string "REFERENCES" props t) t))))

(defun my/vulpea-active-project-p (n)
  "Return non-nil if N is an `active' or `stuck' `project' note."
  (and (vulpea-schema-applies-p n 'project)
       (let ((status (downcase (or (libro-schema-note-raw-prop n "status") "active"))))
         (member status '("active" "stuck")))))

(defun my/vulpea-live-project-p (n)
  "Return non-nil if N is a non-archived `project' note (`active', `stuck', `paused', `done')."
  (and (vulpea-schema-applies-p n 'project)
       (let ((status (downcase (or (libro-schema-note-raw-prop n "status") "active"))))
         (not (string= status "archived")))))

(defun my/vulpea-archived-project-p (n)
  "Return non-nil if N is an `archived' `project' note."
  (and (vulpea-schema-applies-p n 'project)
       (let ((status (downcase (or (libro-schema-note-raw-prop n "status") ""))))
         (string= status "archived"))))

(defvar my/vulpea-collection-views nil
  "Schema-driven collection views for `vulpea-ui-collection'.")

(setq my/vulpea-collection-views
  `(("active-projects"
     :filter (:level 0 :predicate ,#'my/vulpea-active-project-p)
     :columns ((title :width 36)
               (property "STATUS" :name "Status" :width 10)
               (property "PRIORITY" :name "Pri" :width 5)
               (property "AREA" :name "Area" :width 11)
               (property "OUTCOME" :name "Desired Outcome" :width 36)
               backlinks)
     :sort ("Pri" . nil))
    ("projects"
     :filter (:level 0 :predicate ,#'my/vulpea-live-project-p)
     :columns ((title :width 36)
               (property "STATUS" :name "Status" :width 10)
               (property "PRIORITY" :name "Pri" :width 5)
               (property "AREA" :name "Area" :width 11)
               (property "OUTCOME" :name "Desired Outcome" :width 34)
               tags
               backlinks)
     :sort ("Status" . nil))
    ("archived-projects"
     :filter (:level 0 :predicate ,#'my/vulpea-archived-project-p)
     :columns ((title :width 38)
               (property "AREA" :name "Area" :width 11)
               (property "OUTCOME" :name "Outcome" :width 38)
               tags
               backlinks)
     :sort ("Title" . nil))
    ("areas"
     :filter (:level 0 :predicate ,(lambda (n) (vulpea-schema-applies-p n 'area)))
     :columns ((title :width 36)
               tags
               links
               backlinks)
     :sort ("Title" . nil))
    ("books"
     :filter (:level 0 :predicate ,(lambda (n) (vulpea-schema-applies-p n 'book)))
     :columns ((title :width 44)
               (property "AUTHOR" :name "Author" :width 24)
               (property "YEAR" :name "Year" :width 6)
               (property "PUBLISHER" :name "Publisher" :width 20)
               tags
               backlinks)
     :sort ("Title" . nil))
    ("articles"
     :filter (:level 0 :predicate ,(lambda (n)
                                     (and (libro-schema-bib-note-p n)
                                          (not (eq (libro-schema-infer-btype n) 'book)))))
     :columns ((title :width 44)
               (property "AUTHOR" :name "Author" :width 24)
               (property "YEAR" :name "Year" :width 6)
               (property "VENUE" :name "Journal / Venue" :width 24)
               tags
               backlinks)
     :sort ("Title" . nil))
    ("people"
     :filter (:level 0 :predicate ,(lambda (n) (vulpea-schema-applies-p n 'person)))
     :columns ((title :width 32)
               (property "YEARS" :name "Years" :width 14)
               (property "WIKIDATA" :name "Wikidata" :width 16)
               tags
               links
               backlinks)
     :sort ("Title" . nil))
    ("concepts"
     :filter (:level 0 :predicate ,(lambda (n) (vulpea-schema-applies-p n 'concept)))
     :columns ((title :width 40)
               aliases
               tags
               links
               backlinks)
     :sort ("Title" . nil))
    ("movies"
     :filter (:predicate ,(lambda (n) (vulpea-schema-applies-p n 'movie)))
     :columns ((title :width 40)
               (property "YEAR" :name "Year" :width 6)
               (property "RATING" :name "Rating" :width 8)
               (property "WIKIDATA" :name "Wikidata" :width 16)
               backlinks)
     :sort ("Title" . nil))
    ("tv"
     :filter (:predicate ,(lambda (n) (vulpea-schema-applies-p n 'tv)))
     :columns ((title :width 40)
               (property "YEAR" :name "Year" :width 6)
               (property "RATING" :name "Rating" :width 8)
               (property "WIKIDATA" :name "Wikidata" :width 16)
               backlinks)
     :sort ("Title" . nil))
    ("bookmarks"
     :filter (:level 0 :predicate ,#'my/vulpea-bookmark-note-p)
     :columns ((title :width 48)
               (property "REFERENCES" :name "URL / Ref" :width 36)
               tags
               backlinks)
     :sort ("Title" . nil))
    ("journal"
     :filter (:level 0 :predicate ,(lambda (n)
                                     (or (and (member "journal" (vulpea-note-tags n))
                                              (not (member "periodical" (vulpea-note-tags n))))
                                         (string-match-p "/Daily/" (vulpea-note-path n)))))
     :columns ((title :width 16)
               (property "MOOD" :name "Mood" :width 14)
               (property "EXERCISE" :name "Exercise" :width 16)
               (property "DRINKS" :name "Drinks" :width 10)
               (property "PHONE" :name "Phone" :width 10)
               links
               backlinks)
     :sort ("Title" . t))))

(setq vulpea-ui-collection-views my/vulpea-collection-views)
(with-eval-after-load 'vulpea-ui
  (setq vulpea-ui-collection-views my/vulpea-collection-views))

;;; ----------------------------------------------------------------------------
;;; Diagnostic and Migration Helpers
;;; ----------------------------------------------------------------------------

(defun my/vulpea-schema-summary ()
  "Display collection health overview across registered schemas."
  (interactive)
  (let ((health (vulpea-schema-collection-health))
        (buf (get-buffer-create "*Vulpea Schema Health*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "%-12s | %8s | %8s | %8s\n" "Schema" "Covered" "Valid" "Invalid"))
      (insert (make-string 44 ?-) "\n")
      (dolist (h health)
        (let ((name (vulpea-schema-health-schema h))
              (cov (vulpea-schema-health-covered h))
              (inv (vulpea-schema-health-invalid h)))
          (insert (format "%-12s | %8d | %8d | %8d\n"
                          name cov (- cov inv) inv))))
      (goto-char (point-min))
      (special-mode))
    (pop-to-buffer buf)))

(defun my/vulpea-fix-movie-years (&optional dry-run)
  "Extract release years from headings like '** Title (YYYY)' in movies.org
and write them to the :YEAR: property in :PROPERTIES:."
  (interactive "P")
  (let ((movies-file (expand-file-name "movies.org" vulpea-directory))
        (count 0))
    (with-current-buffer (find-file-noselect movies-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward "^\\*\\* +\\(.+?\\) +(\\([12][0-9]\\{3\\}\\))" nil t)
         (let ((year (match-string 2))
               (end-pos (point)))
           (org-back-to-heading t)
           (unless (org-entry-get (point) "YEAR")
             (cl-incf count)
             (unless dry-run
               (org-entry-put (point) "YEAR" year)))
           (goto-char end-pos)
           (forward-line 1))))
      (if dry-run
          (message "Dry-run: found %d movies that can have :YEAR: populated." count)
        (save-buffer)
        (message "Updated %d movies with :YEAR: property." count)))))

(defun my/vulpea-fix-tv-years (&optional dry-run)
  "Extract first-aired years from headings like '** Title (YYYY)' in tv.org
and write them to the :YEAR: property in :PROPERTIES:."
  (interactive "P")
  (let ((tv-file (expand-file-name "20220427210214-tv.org" vulpea-directory))
        (count 0))
    (with-current-buffer (find-file-noselect tv-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward "^\\*\\* +\\(.+?\\) +(\\([12][0-9]\\{3\\}\\))" nil t)
         (let ((year (match-string 2))
               (end-pos (point)))
           (org-back-to-heading t)
           (unless (org-entry-get (point) "YEAR")
             (cl-incf count)
             (unless dry-run
               (org-entry-put (point) "YEAR" year)))
           (goto-char end-pos)
           (forward-line 1))))
      (if dry-run
          (message "Dry-run: found %d TV shows that can have :YEAR: populated." count)
        (save-buffer)
        (message "Updated %d TV shows with :YEAR: property." count)))))

(defun my/vulpea-fix-person-years (&optional dry-run)
  "Sync file-level '#+years:' keyword into the :YEARS: property drawer for person notes."
  (interactive "P")
  (let ((notes (vulpea-db-query (lambda (n) (= (vulpea-note-level n) 0))))
        (count 0))
    (dolist (n notes)
      (let* ((path (vulpea-note-path n))
             (years (my/vulpea-get-file-keyword path "years")))
        (when (and years (not (assoc "YEARS" (vulpea-note-properties n))))
          (cl-incf count)
          (unless dry-run
            (with-current-buffer (find-file-noselect path)
              (org-with-wide-buffer
               (goto-char (point-min))
               (org-entry-put (point-min) "YEARS" years)
               (save-buffer)))))))
    (if dry-run
        (message "Dry-run: found %d person notes that can have :YEARS: populated." count)
      (message "Updated %d person notes with :YEARS: property." count))))

(defun my/vulpea-fix-book-years-from-cite (&optional dry-run)
  "Extract 4-digit publication years from citation keys or custom IDs for books
and write them to the :YEAR: property in :PROPERTIES:."
  (interactive "P")
  (let ((notes (vulpea-db-query (lambda (n) (= (vulpea-note-level n) 0))))
        (count 0))
    (dolist (n notes)
      (when (funcall (vulpea-schema-predicate (vulpea-schema-get 'book)) n)
        (let* ((props (vulpea-note-properties n))
               (has-year (or (assoc "YEAR" props) (assoc "year" props)))
               (cite (concat (or (cdr (assoc "REFERENCES" props)) "") " "
                             (or (cdr (assoc "CUSTOM_ID" props)) ""))))
          (when (and (not has-year)
                     (string-match "\\b\\(18[0-9]\\{2\\}\\|19[0-9]\\{2\\}\\|20[0-2][0-9]\\)\\b" cite))
            (let ((year (match-string 1 cite))
                  (path (vulpea-note-path n)))
              (cl-incf count)
              (unless dry-run
                (with-current-buffer (find-file-noselect path)
                  (org-with-wide-buffer
                   (goto-char (point-min))
                   (org-entry-put (point-min) "YEAR" year)
                   (save-buffer)))))))))
    (if dry-run
        (message "Dry-run: found %d books where :YEAR: can be populated from cite key." count)
      (message "Updated %d books with :YEAR: property from cite key." count))))

(provide 'private-vulpea-schemas)
;;; schemas.el ends here
