;;; wikidata.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Jonathan Reeve
;;
;; Author: Jonathan Reeve <https://github.com/jon>
;; Maintainer: Jonathan Reeve <jon@jon-laptop>
;; Created: Aprilo 17, 2021
;; Modified: Aprilo 17, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/jon/wikidata
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'request)

(request
 "http://httpbin.org/get"
 :params '(("key" . "value") ("key2" . "value2"))
 :parser 'json-read
 :success (cl-function
             (lambda (&key data &allow-other-keys)
               (setq json data)))
 )

(message json)

(provide 'wikidata)

;;; wikidata.el ends here
