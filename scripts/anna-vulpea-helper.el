;;; anna-vulpea-helper.el --- Helper for Anna's Archive integration -*- lexical-binding: t; -*-

(add-to-list 'load-path "/home/jon/Programaroj/libro")
(require 'vulpea)
(require 'json)
(require 'libro)

(defun my/vulpea-create-anna-note (payload-file)
  "Create a schema-compliant `libro' Vulpea note from metadata in PAYLOAD-FILE."
  (let* ((data (with-temp-buffer
                 (insert-file-contents payload-file)
                 (json-parse-buffer :object-type 'alist)))
         (meta `((CUSTOM_ID . ,(alist-get 'citekey data))
                 (BTYPE . "book")
                 (TITLE . ,(alist-get 'title data))
                 (AUTHOR . ,(alist-get 'author data))
                 (PUBLISHER . ,(alist-get 'publisher data))
                 (YEAR . ,(alist-get 'year data))
                 (ISBN . ,(alist-get 'isbn data))
                 (MD5 . ,(alist-get 'md5 data))
                 (LANG . ,(alist-get 'lang data))
                 (URL . ,(alist-get 'url data))
                 (NOTER_DOCUMENT . ,(alist-get 'noter_doc data))
                 (ABSTRACT . ,(alist-get 'description data)))))
    (libro-create-from-metadata meta t)))

(provide 'anna-vulpea-helper)
;;; anna-vulpea-helper.el ends here
