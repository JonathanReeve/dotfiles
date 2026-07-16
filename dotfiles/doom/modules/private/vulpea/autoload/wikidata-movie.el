;;; modules/private/vulpea/autoload/wikidata-movie.el -*- lexical-binding: t; -*-

(require 'json)
(require 'url)

(defun my/wikidata-get-json (url)
  "Fetch JSON from URL and return it as an alist."
  (let ((url-request-headers '(("User-Agent" . "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AntigravityMovieCapture/1.0"))))
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
  "Get movie details for ENTITY-ID."
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
         (director (if director-id (my/wikidata-get-label director-id) "Unknown"))
         ;; Publication date (P577)
         (date-claims (cdr (assoc 'P577 claims)))
         (year (when date-claims
                 (let* ((mainsnak (cdr (assoc 'mainsnak (car date-claims))))
                        (datavalue (cdr (assoc 'datavalue mainsnak)))
                        (value (cdr (assoc 'value datavalue)))
                        (time-str (cdr (assoc 'time value))))
                   (when (and time-str (string-prefix-p "+" time-str))
                     (substring time-str 1 5))))))
    (list :id entity-id :label label :director director :year year)))

(defun my/wikidata-resolve-id (url-or-title)
  "Resolve Wikidata ID from URL-OR-TITLE."
  (cond
   ;; 1. Empty input
   ((or (not url-or-title) (string-empty-p url-or-title))
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
   
   ;; 4. Search by title
   (t
    (let* ((clean-title (replace-regexp-in-string " - Wikipedia\\| - Wikidata\\| (film)" "" url-or-title))
           (api-url (format "https://www.wikidata.org/w/api.php?action=wbsearchentities&search=%s&language=en&format=json&type=item"
                            (url-hexify-string clean-title)))
           (data (my/wikidata-get-json api-url))
           (search (cdr (assoc 'search data))))
      (when search
        (cdr (assoc 'id (car search))))))))

(defun my/extract-url-from-annotation (annotation)
  "Extract the raw URL from an Org-mode link annotation."
  (cond
   ((null annotation) nil)
   ((string-match "\\[\\[\\([^]]+\\)\\]\\[" annotation)
    (match-string 1 annotation))
   ((string-match "\\[\\[\\([^]]+\\)\\]\\]" annotation)
    (match-string 1 annotation))
   (t annotation)))

;;;###autoload
(defun my/capture-movie-template ()
  "Generate the movie template for org-capture using Wikidata."
  (message "DEBUG: org-capture-plist is %S" org-capture-plist)
  (message "DEBUG: org-store-link-plist is %S" org-store-link-plist)
  (message "DEBUG: my/capture-movie-template - annotation: %S, link: %S"
           (org-capture-get :annotation) (org-capture-get :link))
  (let* ((annotation (or (plist-get org-store-link-plist :annotation)
                         (org-capture-get :annotation)))
         (link (or (plist-get org-store-link-plist :link)
                   (org-capture-get :link)))
         (url-or-title (or (my/extract-url-from-annotation annotation) link))
         (url-or-title (or url-or-title (read-string "URL or Title: ")))
         (entity-id (my/wikidata-resolve-id url-or-title)))
    (if (not entity-id)
        (error "Could not find Wikidata entity for: %s" url-or-title)
      (let* ((details (my/wikidata-get-details entity-id))
             (label (plist-get details :label))
             (director (plist-get details :director))
             (year (plist-get details :year))
             (title (if year (format "%s (%s)" label year) label))
             ;; Find director node in Vulpea DB
             (director-node (seq-find (lambda (n) (string-match-p (regexp-quote director) (vulpea-note-title n)))
                                       (vulpea-db-query)))
             (director-link (if director-node
                                (format "[[id:%s][%s]]" (vulpea-note-id director-node) director)
                              director))
             ;; Format Esperanto date
             (today (let* ((eo-days '((1 . "lun") (2 . "mar") (3 . "mer") (4 . "ĵaŭ") (5 . "ven") (6 . "sab") (0 . "dim")))
                           (day-num (string-to-number (format-time-string "%w")))
                           (eo-day (cdr (assoc day-num eo-days))))
                      (format-time-string (format "[%%Y-%%m-%%d %s]" eo-day))))
             (body-content (format "%s\n\nDirected by %s.\n\n" today director-link))
             (id (org-id-uuid)))
        (format "* %s\n:PROPERTIES:\n:ID:       %s\n:WIKIDATA: [[wd:%s]]\n:RATING:   ***\n:END:\n%s"
                title id entity-id body-content)))))

;;;###autoload
(defun my/capture-movie-from-url (url &optional title)
  "Trigger movie capture template with URL and TITLE."
  (interactive "sURL: ")
  (let ((org-capture-plist (list :annotation (format "[[%s][%s]]" url (or title url)))))
    (org-capture nil "m")))

(provide 'wikidata-movie)
