;;; getBook.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Jonathan Reeve
;;
;; Author: Jonathan Reeve <https://github.com/jon>
;; Maintainer: Jonathan Reeve <jonathan@jonreeve.com>
;; Created: Majo 31, 2021
;; Modified: Majo 31, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/jon/getBook
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;/home/jon/Dokumentoj/Papers/;;  Description
;;
;;; Code:

(require 'enlive)
(require 'seq)
(url-copy-file "http://google.com")

; asynchronously
(defun print-url (url)
  (url-retrieve url 'print))

(print-url "http://www.gnu.org")

; synchronously
(defun get-url (url)
  (with-current-buffer (url-retrieve-synchronously url) (buffer-string)))

(print )

(setq data )

(with-temp-buffer (get-url "http://www.google.com"))


(insert data)
(defun getBook (query)
"Get a book from a query. Add it to database."
  (interactive)

  (require 'url-util)

  (let ((queryURL (format "https://libgen.is/search.php?req=%s" (url-hexify-string query))))

    (seq-filter
     (lambda (element) (enlive-attr element 'href))
     (enlive-query-all (enlive-fetch queryURL) [td > a]))
        ;; (switch-to-buffer (url-retrieve-synchronously queryURL))
  )
)

(insert (getBook "joy of clojure"))

(message results)

(provide 'getBook)
;;; getBook.el ends here
