;;; modules/private/vulpea/autoload/wikidata-movie.el -*- lexical-binding: t; -*-

(require 'json)
(require 'url)

(defgroup my/wikidata nil
  "Customizations for Wikidata media capture."
  :group 'org)

(defcustom my/wikidata-tv-include-director nil
  "Whether to include director in TV show capture if available.
By default, TV shows do not include a director."
  :type 'boolean
  :group 'my/wikidata)

(defun my/wikidata-get-json (url)
  "Fetch JSON from URL and return it as an alist."
  (let ((url-request-headers '(("User-Agent" . "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AntigravityMediaCapture/1.0 (mailto:jonathan@jonreeve.com)"))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$" nil t) ; skip headers
      (json-parse-buffer :object-type 'alist :array-type 'list))))

(defun my/wikidata-get-label (entity-id)
  "Get label of ENTITY-ID in English."
  (let* ((url (format "https://www.wikidata.org/w/api.php?action=wbgetentities&ids=%s&props=labels&languages=en&format=json" entity-id))
         (data (my/wikidata-get-json url))
         (entities (cdr (assoc 'entities data)))
         (entity (cdr (assoc (intern entity-id) entities)))
         (labels (cdr (assoc 'labels entity)))
         (en (cdr (assoc 'en labels))))
    (if en
        (cdr (assoc 'value en))
      "Unknown")))

(defun my/wikidata-get-details (entity-id)
  "Get media details for ENTITY-ID."
  (let* ((url (format "https://www.wikidata.org/wiki/Special:EntityData/%s.json" entity-id))
         (data (my/wikidata-get-json url))
         (entities (cdr (assoc 'entities data)))
         (entity (cdr (assoc (intern entity-id) entities)))
         (claims (cdr (assoc 'claims entity)))
         (labels (cdr (assoc 'labels entity)))
         (en-label (cdr (assoc 'en labels)))
         (label (if en-label (cdr (assoc 'value en-label)) "Unknown"))
         ;; Director (P57)
         (director-claims (cdr (assoc 'P57 claims)))
         (director-id (when director-claims
                        (let* ((mainsnak (cdr (assoc 'mainsnak (car director-claims))))
                               (datavalue (cdr (assoc 'datavalue mainsnak)))
                               (value (cdr (assoc 'value datavalue))))
                          (cdr (assoc 'id value)))))
         (director (when director-id (my/wikidata-get-label director-id)))
         ;; Publication date (P577), Start time (P580), or Inception (P571)
         (date-claims (or (cdr (assoc 'P577 claims))
                          (cdr (assoc 'P580 claims))
                          (cdr (assoc 'P571 claims))))
         (year (when date-claims
                 (let* ((mainsnak (cdr (assoc 'mainsnak (car date-claims))))
                        (datavalue (cdr (assoc 'datavalue mainsnak)))
                        (value (cdr (assoc 'value datavalue)))
                        (time-str (cdr (assoc 'time value))))
                   (when (and time-str (string-prefix-p "+" time-str))
                     (substring time-str 1 5))))))
    (list :id entity-id :label label :director director :year year)))

(defun my/wikidata-get-person-details (entity-id)
  "Get person details (label, birth year, death year, formatted years) for ENTITY-ID."
  (let* ((url (format "https://www.wikidata.org/wiki/Special:EntityData/%s.json" entity-id))
         (data (my/wikidata-get-json url))
         (entities (cdr (assoc 'entities data)))
         (entity (cdr (assoc (intern entity-id) entities)))
         (claims (cdr (assoc 'claims entity)))
         (labels (cdr (assoc 'labels entity)))
         (en-label (cdr (assoc 'en labels)))
         (label (if en-label (cdr (assoc 'value en-label)) "Unknown"))
         (extract-year
          (lambda (prop-claims)
            (when prop-claims
              (let* ((mainsnak (cdr (assoc 'mainsnak (car prop-claims))))
                     (datavalue (cdr (assoc 'datavalue mainsnak)))
                     (value (cdr (assoc 'value datavalue)))
                     (time-str (cdr (assoc 'time value))))
                (when (and time-str (string-match "^[+-]?0*\\([12][0-9]\\{2,3\\}\\)-" time-str))
                  (match-string 1 time-str))))))
         ;; Date of birth (P569) and Date of death (P570)
         (birth-year (funcall extract-year (cdr (assoc 'P569 claims))))
         (death-year (funcall extract-year (cdr (assoc 'P570 claims))))
         (years (when birth-year
                  (format "%s–%s" birth-year (or death-year "")))))
    (list :id entity-id :label label :birth-year birth-year :death-year death-year :years years)))

(defun my/wikidata-resolve-id (url-or-title &optional media-type target-year)
  "Resolve Wikidata ID from URL-OR-TITLE with optional MEDIA-TYPE and TARGET-YEAR.
MEDIA-TYPE can be :movie, :tv, :person, or nil.
TARGET-YEAR can be a string or number representing the expected year."
  (cond
   ;; 1. Empty input
   ((or (not url-or-title) (string-empty-p (string-trim url-or-title)))
    nil)

   ;; 2. Wikidata URL
   ((string-match "wikidata\\.org/wiki/\\(Q[0-9]+\\)" url-or-title)
    (match-string 1 url-or-title))

   ;; 3. Wikipedia URL
   ((string-match "https?://\\([a-z-]+\\)\\.wikipedia\\.org/wiki/\\([^?#]+\\)" url-or-title)
    (let* ((lang (match-string 1 url-or-title))
           (site (concat lang "wiki"))
           (title (url-unhex-string (match-string 2 url-or-title)))
           (api-url (format "https://www.wikidata.org/w/api.php?action=wbgetentities&sites=%s&titles=%s&format=json"
                            site (url-hexify-string title)))
           (data (my/wikidata-get-json api-url))
           (entities (cdr (assoc 'entities data))))
      (when entities
        (symbol-name (car (mapcar 'car entities))))))

   ;; 4. IMDb URL
   ((string-match "imdb\\.com/title/\\(tt[0-9]+\\)" url-or-title)
    (let* ((imdb-id (match-string 1 url-or-title))
           (query (format "SELECT ?item WHERE { ?item wdt:P345 \"%s\" . }" imdb-id))
           (api-url (format "https://query.wikidata.org/sparql?query=%s&format=json" (url-hexify-string query)))
           (data (my/wikidata-get-json api-url))
           (results (cdr (assoc 'results data)))
           (bindings (cdr (assoc 'bindings results))))
      (when bindings
        (let* ((item-uri (cdr (assoc 'value (cdr (assoc 'item (car bindings)))))))
          (when (string-match "Q[0-9]+" item-uri)
            (match-string 0 item-uri))))))

   ;; 5. Generic HTTP/HTTPS URL
   ((string-match-p "\\`https?://" url-or-title)
    nil)

   ;; 6. Search by title with type-safe disambiguation
   (t
    (let* ((year (or target-year
                     (when (string-match "(\\([12][0-9]\\{3\\}\\))" url-or-title)
                       (match-string 1 url-or-title))))
           (clean-title (replace-regexp-in-string
                         "\\`Watch \\| - Wikipedia\\| - Wikidata\\| - IMDb\\| | Prime Video\\| | Netflix\\| (film)\\| (TV series)\\| (TV Series.*)\\| (miniseries)\\| (TV miniseries)\\|: Season [0-9]+\\| - Season [0-9]+\\| ([12][0-9]\\{3\\})"
                         "" url-or-title))
           (api-url (format "https://www.wikidata.org/w/api.php?action=wbsearchentities&search=%s&language=en&format=json&type=item&limit=10"
                            (url-hexify-string (string-trim clean-title))))
           (data (my/wikidata-get-json api-url))
           (candidates (cdr (assoc 'search data)))
           (filtered
            (cl-remove-if-not
             (lambda (item)
               (let ((desc (downcase (or (cdr (assoc 'description item)) ""))))
                 (pcase media-type
                   (:movie
                    (and (or (string-match-p "film" desc)
                             (string-match-p "movie" desc)
                             (string-match-p "documentary" desc))
                         (not (string-match-p "television series\\|tv series\\|soundtrack\\|video game\\|given name\\|production company" desc))))
                   (:tv
                    (and (or (string-match-p "television series\\|tv series\\|miniseries\\|television program\\|animated series" desc))
                         (not (string-match-p "soundtrack\\|video game\\|given name\\|film" desc))))
                   (:person
                    (not (string-match-p "film\\|television\\|series\\|album\\|song\\|genus\\|video game" desc)))
                   (_ t))))
             candidates))
           ;; If year is known, prefer candidate matching the year
           (matched-candidate
            (or (when year
                  (seq-find (lambda (item)
                              (string-match-p (regexp-quote (format "%s" year))
                                              (or (cdr (assoc 'description item)) "")))
                            filtered))
                (car filtered)
                (car candidates))))
      (when matched-candidate
        (cdr (assoc 'id matched-candidate)))))))

(defun my/extract-url-and-title-from-annotation (annotation)
  "Extract (URL . TITLE) from ANNOTATION."
  (cond
   ((null annotation) '(nil . nil))
   ((string-match "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" annotation)
    (cons (match-string 1 annotation) (match-string 2 annotation)))
   ((string-match "\\[\\[\\([^]]+\\)\\]\\]" annotation)
    (cons (match-string 1 annotation) nil))
   (t (cons annotation nil))))

(defun my/extract-url-from-annotation (annotation)
  "Extract the raw URL from an Org-mode link annotation."
  (car (my/extract-url-and-title-from-annotation annotation)))

(defun my/format-eo-date ()
  "Format current date with Esperanto day abbreviation, e.g. [2026-10-13 mar]."
  (let* ((eo-days '((1 . "lun") (2 . "mar") (3 . "mer") (4 . "ĵaŭ") (5 . "ven") (6 . "sab") (0 . "dim")))
         (day-num (string-to-number (format-time-string "%w")))
         (eo-day (cdr (assoc day-num eo-days))))
    (format-time-string (format "[%%Y-%%m-%%d %s]" eo-day))))

(defun my/capture-wikidata-media-template (&optional media-type)
  "Generate template string for org-capture using Wikidata.
MEDIA-TYPE can be :movie (default) or :tv."
  (let* ((media-type (or media-type :movie))
         (annotation (or (plist-get org-store-link-plist :annotation)
                         (org-capture-get :annotation)))
         (link (or (plist-get org-store-link-plist :link)
                   (org-capture-get :link)))
         (url-and-title (my/extract-url-and-title-from-annotation annotation))
         (url (or (car url-and-title) link))
         (page-title (cdr url-and-title))
         (url-or-title (or url page-title))
         (entity-id (or (when url (my/wikidata-resolve-id url media-type))
                        (when page-title (my/wikidata-resolve-id page-title media-type))
                        (my/wikidata-resolve-id (read-string "URL or Title: ") media-type))))
    (if (not entity-id)
        (error "Could not find Wikidata entity for: %s" (or url-or-title "unknown input"))
      (let* ((details (my/wikidata-get-details entity-id))
             (label (plist-get details :label))
             (director (plist-get details :director))
             (year (plist-get details :year))
             (title (if year (format "%s (%s)" label year) label))
             (today (my/format-eo-date))
             (body-content
              (cond
               ;; TV show: doesn't have a director by default
               ((eq media-type :tv)
                (if (and my/wikidata-tv-include-director
                         director
                         (not (string= director "Unknown")))
                    (let* ((director-node (seq-find (lambda (n) (string-match-p (regexp-quote director) (vulpea-note-title n)))
                                                    (vulpea-db-query)))
                           (director-link (if director-node
                                              (format "[[id:%s][%s]]" (vulpea-note-id director-node) director)
                                            director)))
                      (format "%s\n\nDirected by %s.\n\n" today director-link))
                  (format "%s\n\n" today)))
               ;; Movies: include director (defaults to "Unknown")
               (t
                (let* ((dir-name (or director "Unknown"))
                       (director-node (seq-find (lambda (n) (string-match-p (regexp-quote dir-name) (vulpea-note-title n)))
                                                (vulpea-db-query)))
                       (director-link (if director-node
                                          (format "[[id:%s][%s]]" (vulpea-note-id director-node) dir-name)
                                        dir-name)))
                  (format "%s\n\nDirected by %s.\n\n" today director-link)))))
             (id (org-id-uuid))
             (year-prop (if (and year (not (string-empty-p year)))
                            (format ":YEAR:     %s\n" year)
                          "")))
        (format "* %s\n:PROPERTIES:\n:ID:       %s\n%s:WIKIDATA: [[wd:%s]]\n:RATING:   ***\n:END:\n%s"
                title id year-prop entity-id body-content)))))

;;;###autoload
(defun my/capture-movie-template ()
  "Generate the movie template for org-capture using Wikidata."
  (my/capture-wikidata-media-template :movie))

;;;###autoload
(defun my/capture-tv-template ()
  "Generate the TV show template for org-capture using Wikidata."
  (my/capture-wikidata-media-template :tv))

;;;###autoload
(defun my/capture-movie-from-url (url &optional title)
  "Trigger movie capture template with URL and TITLE."
  (interactive "sURL: ")
  (let ((org-capture-plist (list :annotation (format "[[%s][%s]]" url (or title url)))))
    (org-capture nil "m")))

;;;###autoload
(defun my/capture-tv-from-url (url &optional title)
  "Trigger TV show capture template with URL and TITLE."
  (interactive "sURL: ")
  (let ((org-capture-plist (list :annotation (format "[[%s][%s]]" url (or title url)))))
    (org-capture nil "T")))

(provide 'wikidata-movie)
