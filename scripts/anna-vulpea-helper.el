;;; anna-vulpea-helper.el --- Helper for Anna's Archive integration -*- lexical-binding: t; -*-

(require 'vulpea)
(require 'json)

(defun my/vulpea-create-anna-note (payload-file)
  "Create a note from metadata in PAYLOAD-FILE.
Renames/moves are handled by the calling script, this just creates the note."
  (let* ((data (with-temp-buffer
                 (insert-file-contents payload-file)
                 (json-parse-buffer :object-type 'alist)))
         (title (alist-get 'title data))
         (citekey (alist-get 'citekey data))
         (author (alist-get 'author data))
         (url (alist-get 'url data))
         (noter-doc (alist-get 'noter_doc data))
         (md5 (alist-get 'md5 data))
         (isbn (alist-get 'isbn data))
         (lang (alist-get 'lang data))
         (publisher (alist-get 'publisher data))
         (year (alist-get 'year data))
         (description (alist-get 'description data))
         ;; Use your standard reference template properties
         (filename (concat citekey ".org"))
         (path (expand-file-name filename vulpea-directory)))
    
    (if (file-exists-p path)
        (message "Note already exists at %s" path)
      (vulpea-create
       title
       filename
       :tags '("personal" "reference")
       :properties `(("REFERENCES" . ,(concat "cite:" citekey))
                     ("CUSTOM_ID" . ,citekey)
                     ("AUTHOR" . ,author)
                     ("URL" . ,url)
                     ("NOTER_DOCUMENT" . ,noter-doc)
                     ("MD5" . ,md5)
                     ("ISBN" . ,isbn)
                     ("LANG" . ,lang)
                     ("PUBLISHER" . ,publisher)
                     ("YEAR" . ,year))
       :head (concat (format "#+created: %s\n#+last-modified: %s\n\n- keywords :: \n- related ::\n\n"
                             (format-time-string "[%Y-%m-%d %a %H:%M]")
                             (format-time-string "[%Y-%m-%d %a %H:%M]"))
                     "* " title "\n\n" description)))))

(provide 'anna-vulpea-helper)
