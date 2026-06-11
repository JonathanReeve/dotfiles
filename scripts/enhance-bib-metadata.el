;;; enhance-bib-metadata.el --- Enhance bibliographic metadata natively -*- lexical-binding: t; -*-

(require 'url)
(require 'json)
(require 'org-element)
(require 'vulpea)

(defun my/bib-enhance-fetch-crossref (doi)
  "Fetch metadata from CrossRef for DOI."
  (let ((url (format "https://api.crossref.org/works/%s" doi))
        (metadata nil))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (when (re-search-forward "^$" nil t)
        (let* ((data (json-parse-buffer :object-type 'alist))
               (msg (alist-get 'message data)))
          (when msg
            (let ((title (elt (alist-get 'title msg) 0))
                  (authors (alist-get 'author msg))
                  (date (alist-get 'published-print msg)))
              (setq metadata
                    (list
                     (cons 'TITLE title)
                     (cons 'AUTHOR (when authors
                                     (mapconcat (lambda (a) 
                                                  (format "%s, %s" 
                                                          (alist-get 'family a)
                                                          (alist-get 'given a)))
                                                authors "; ")))
                     (cons 'YEAR (when date
                                   (format "%s" (elt (elt (alist-get 'date-parts date) 0) 0))))))))))
      (kill-buffer (current-buffer)))
    metadata))

(defun my/bib-enhance-fetch-google-books-id (id)
  "Fetch metadata from Google Books for ID."
  (let ((url (format "https://www.googleapis.com/books/v1/volumes/%s" id))
        (metadata nil))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (when (re-search-forward "^$" nil t)
        (let ((data (json-parse-buffer :object-type 'alist)))
          (unless (alist-get 'error data)
            (when-let* ((info (alist-get 'volumeInfo data)))
              (let ((title (alist-get 'title info))
                    (authors (alist-get 'authors info))
                    (date (alist-get 'publishedDate info))
                    (link (alist-get 'infoLink info))
                    (pub (alist-get 'publisher info)))
                (setq metadata
                      (list
                       (cons 'TITLE title)
                       (cons 'AUTHOR (when (sequencep authors)
                                       (mapconcat #'identity authors "; ")))
                       (cons 'YEAR (when (and date (> (length date) 3))
                                     (substring date 0 4)))
                       (cons 'URL link)
                       (cons 'PUBLISHER pub))))))))
      (kill-buffer (current-buffer)))
    metadata))

(defun my/bib-enhance-fetch-google-books (query)
  "Fetch metadata from Google Books for QUERY."
  (let ((url (format "https://www.googleapis.com/books/v1/volumes?q=%s&maxResults=1" 
                     (url-hexify-string query)))
        (metadata nil))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char (point-min))
      (when (re-search-forward "^$" nil t)
        (let* ((data (json-parse-buffer :object-type 'alist))
               (total (alist-get 'totalItems data)))
          (unless (alist-get 'error data)
            (when (and total (> total 0))
              (let* ((item (elt (alist-get 'items data) 0))
                     (info (alist-get 'volumeInfo item)))
                (when info
                  (let ((title (alist-get 'title info))
                        (authors (alist-get 'authors info))
                        (date (alist-get 'publishedDate info))
                        (link (alist-get 'infoLink info))
                        (pub (alist-get 'publisher info)))
                    (setq metadata
                          (list
                           (cons 'TITLE title)
                           (cons 'AUTHOR (when (sequencep authors)
                                           (mapconcat #'identity authors "; ")))
                           (cons 'YEAR (when (and date (> (length date) 3))
                                         (substring date 0 4)))
                           (cons 'URL link)
                           (cons 'PUBLISHER pub))))))))))
      (kill-buffer (current-buffer)))
    metadata))

(defun my/bib-enhance-buffer ()
  "Enhance bibliographic metadata for the current buffer."
  (interactive)
  (let* ((props (org-entry-properties (point-min)))
         (doi (cdr (assoc "DOI" props)))
         (isbn (cdr (assoc "ISBN" props)))
         (title (cdr (assoc "TITLE" props)))
         (author (cdr (assoc "AUTHOR" props)))
         (url-prop (cdr (assoc "URL" props)))
         (metadata nil))
    (message "Enhancing current buffer...")
    (let ((gb-id (when (and url-prop (string-match "id=\\([^&]+\\)" url-prop))
                   (match-string 1 url-prop))))
      (cond
       (gb-id (setq metadata (my/bib-enhance-fetch-google-books-id gb-id)))
       (doi (setq metadata (my/bib-enhance-fetch-crossref doi)))
       (isbn (setq metadata (my/bib-enhance-fetch-google-books (format "isbn:%s" isbn))))
       (title (setq metadata (my/bib-enhance-fetch-google-books 
                              (format "intitle:%s inauthor:%s" title (or author "")))))))
    (if metadata
        (let ((modified nil))
          (dolist (pair metadata)
            (let ((key (symbol-name (car pair)))
                  (val (cdr pair)))
              (when (and val (not (org-entry-get (point-min) key t)))
                (org-entry-put (point-min) key val)
                (setq modified t))))
          (if modified
              (message "Updated bibliographic metadata.")
            (message "No new metadata found or properties already present.")))
      (message "Could not find metadata for this note (API error or quota exceeded)."))))

(defun my/bib-enhance-note (node)
  "Enhance metadata for VULPEA-NOTE NODE."
  (with-current-buffer (find-file-noselect (vulpea-note-path node))
    (my/bib-enhance-buffer)
    (when (buffer-modified-p)
      (save-buffer))))

(defun my/bib-enhance-all ()
  "Enhance all bibliographic notes in the Vulpea database."
  (interactive)
  (let ((nodes (vulpea-db-query)))
    (dolist (node nodes)
      (let ((props (vulpea-note-properties node)))
        (when (or (assoc "REFERENCES" props)
                  (assoc "TITLE" props))
          (condition-case err
              (my/bib-enhance-note node)
            (error (message "Error enhancing %s: %s" (vulpea-note-title node) err))))))))

(provide 'enhance-bib-metadata)
