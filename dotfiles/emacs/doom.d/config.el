;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq vc-follow-symlinks t) ;; Always follow symlinks.

;; Get system notifications through libnotify
(setq alert-default-style 'libnotify)

;; Don't prompt when opening journal or other large files
;(setq large-file-warning-threshold 20000000)

;; Org Mode
(setq org-startup-indented t)
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE" "CANCELED")))
(setq evil-org-key-theme '(textobjects navigation additional insert todo))
(setq org-default-priority ?C)
(setq org-lowest-priority ?G)

;; Show clock tables in hours, not days.
(setq org-duration-format 'h:mm)

;; Clock break time in pomodoro
(setq org-pomodoro-clock-break t)

(defun org-journal-new-entry ()
  "Inserts header with inactive timestamp, hours and minutes.
A custom journal helper function."
  (interactive)
  (org-insert-heading)
  (org-insert-time-stamp (current-time) t t))

;; Enable visual-line-mode by default in Org Mode.
(add-hook 'org-mode-hook 'visual-line-mode)

;; Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-normal-state-map "n" 'evil-next-visual-line)
(define-key evil-normal-state-map "e" 'evil-previous-visual-line)
;; Also in visual mode
(define-key evil-visual-state-map "n" 'evil-next-visual-line)
(define-key evil-visual-state-map "e" 'evil-previous-visual-line)

;; Python in Org Mode
(setq python-shell-completion-native-disabled-interpreters "python")

;; Org Calendar and Diary
(setq diary-file "~/Dropbox/Org/diary")
(setq org-agenda-include-diary t)
;;(setq org-agenda-window-setup 'only-window)
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (calendar-set-date-style 'iso)))
(add-hook 'diary-mode-hook 'diary-fancy-display-mode)

;; Prose linting
;; (require 'flycheck-vale)
;; (flycheck-vale-setup)

(setq org-modules '(org-bibtex org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m org-protocol))

;; Org Agenda
(setq org-agenda-files (list "~/Dropbox/Org/Projects/")
      org-projectile-file "~/Dropbox/Org/Projects/todo.org")

(setq org-refile-targets '((nil :maxlevel . 9)
                            (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling

;; Adapted from http://stackoverflow.com/a/12751732/584121
(require 'org-protocol)
(setq org-protocol-default-template-key "l")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "/home/jon/Dropbox/Org/notes.org" "Tasks")
          "* TODO %?  %i\n  %a")
        ("m" "Movie" entry (file+headline "/home/jon/Dropbox/Org/movies.org" "To Watch")
          "* %a\n %?\n %i")
        ("l" "Link" entry (file+olp "/home/jon/Dropbox/Org/notes.org" "Web Links")
          "* %a\n %?\n %i")
        ("s" "Schedule" entry (file "/home/jon/Dropbox/Org/Projects/schedule.org")
          "* %?\n :PROPERTIES:\n :LOCATION:\n :END:\n %a\n %i")
        ))

;; Disable holidays. Is there an easier way of doing this?
(setq holiday-christian-holidays nil
      holiday-islamic-holidays nil
      holiday-bahai-holidays nil
      holiday-oriental-holidays nil
      holiday-hebrew-holidays nil)

;; Bibliography
(setq reftex-default-bibliography '("~/Dropbox/Columbia/Dissertation/references.bib"
                                    "~/Dropbox/Papers/library.bib" "~/Dropbox/Papers/zotero.bib")
      org-ref-default-bibliography '("~/Dropbox/Papers/library.bib"
        "~/Dropbox/Columbia/Dissertation/references.bib")
      org-ref-pdf-directory "~/Dropbox/Papers" ;; keep the final slash off
      org-ref-bibliography-notes "~/Dropbox/Org/Projects/books.org"
      bibtex-completion-pdf-field "file"
      org-ref-get-pdf-filename-function 'org-ref-get-zotero-pdf-filename)

(setq org-ref-note-title-format
  "** TODO %y - %9a - %t
:PROPERTIES:
:Custom_ID: %k
:AUTHOR: %9a
:JOURNAL: %j
:AVAILABILITY:
:YEAR: %y
:END:
")
(setq org-ref-open-pdf-function
      (lambda (fpath)
        (start-process "zathura" "*helm-bibtex-zathura*" "/usr/bin/zathura" fpath)))

(defun org-ref-get-zotero-pdf-filename (key)
  "Return the pdf filename indicated by zotero file field.
Argument KEY is the bibtex key."
  (let* ((results (org-ref-get-bibtex-key-and-file key))
          (bibfile (cdr results))
          entry)
    (with-temp-buffer
      (insert-file-contents bibfile)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-search-entry key nil 0)
      (setq entry (bibtex-parse-entry))
      (let ((e (org-ref-reftex-get-bib-field "file" entry)))
        (if (> (length e) 4)
            (let ((clean-field (replace-regexp-in-string "/+" "/" e)))
              (let ((first-file (car (split-string clean-field ";" t))))
                (concat "" first-file)))
          (message "PDF filename not found.")
          )))))

;; Override this function.
(defun org-ref-open-bibtex-pdf ()
  "Open pdf for a bibtex entry, if it exists.
assumes point is in
the entry of interest in the bibfile.  but does not check that."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((bibtex-expand-strings t)
            (entry (bibtex-parse-entry t))
            (key (reftex-get-bib-field "=key=" entry))
            (pdf (org-ref-get-zotero-pdf-filename key)))
      (message "%s" pdf)
      (if (file-exists-p pdf)
          ;; (org-open-link-from-string (format "[[file:%s]]" pdf))
          (shell-command (format "zathura %s" pdf))
        (ding)))))

;; Open PDFs with system viewer
(delete '("\\.pdf\\'" . default) org-file-apps)
(add-to-list 'org-file-apps '("\\.pdf\\'" . system))

;; Org-brain
(setq org-brain-path "~/Dropbox/Org/Brain")

;; Markdown
(add-hook 'markdown-mode 'visual-line-mode)

(require 'auth-source-pass)
(auth-source-pass-enable)

;; Mail
;; Send email via Gmail:
(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com")

(add-hook 'mu4e-view-mode-hook 'visual-line-mode)
(setq mu4e-html2text-command "w3m -T text/html")

;; configure orgmode support in mu4e
;; (require 'org-mu4e)
;; when mail is sent, automatically convert org body to HTML
(setq org-mu4e-convert-to-html t)

;; Better looking HTML mail
;;(setq shr-color-visible-luminance-min 80)
;;(setq shr-use-colors nil)

(require 'mu4e)
(setq mu4e-contexts
      `( ,(make-mu4e-context
            :name "Gmail"
            :enter-func (lambda () (mu4e-message "Switch to Gmail."))
            ;; leave-func not defined
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches msg
                                                                :to "jon.reeve@gmail.com")))
            :vars '(  ( user-mail-address  . "jon.reeve@gmail.com" )
                      ( user-full-name     . "Jonathan Reeve" )
                      ( mu4e-sent-folder   . "/gmail/[Gmail]/.Sent Mail" )
                      ( mu4e-drafts-folder . "/gmail/[Gmail]/.Drafts" )
                      ( smtpmail-smtp-user . "jon.reeve" )

                      ;; ( mu4e-compose-signature .
                      ;;                          (concat
                      ;;                           "Alice Derleth\n"
                      ;;                           "Lauttasaari, Finland\n"))
                      ))
          ,(make-mu4e-context
            :name "Columbia"
            :enter-func (lambda () (mu4e-message "Switch to Columbia."))
            ;; leave-fun not defined
            :match-func (lambda (msg)
                          (when msg
                            (or
                            (mu4e-message-contact-field-matches msg
                                                                :to "jonathan.reeve@columbia.edu")
                            (mu4e-message-contact-field-matches msg
                                                                :to "jpr2152@columbia.edu"))
                            ))
            :vars '(  ( user-mail-address   . "jonathan.reeve@columbia.edu" )
                      ( user-full-name      . "Jonathan Reeve" )
                      ( mu4e-sent-folder    . "/columbia/[Gmail]/.Sent Mail" )
                      ( mu4e-drafts-folder  . "/columbia/[Gmail]/.Drafts" )
                      ( smtpmail-smtp-user  . "jpr2152@columbia.edu" )

                      ;; ( mu4e-compose-signature .
                      ;;                          (concat
                      ;;                           "Prof. Alice Derleth\n"
                      ;;                           "Miskatonic University, Dept. of Occult Sciences\n"))
                      ))))

;; start with the first (default) context;
;; default is to ask-if-none (ask when there's no context yet, and none match)
(setq mu4e-context-policy 'pick-first)

;; compose with the current context is no context matches;
;; default is to ask
(setq mu4e-compose-context-policy nil)

(setq mu4e-maildir "~/Mail"
      mu4e-trash-folder "/Trash"
      mu4e-refile-folder "/Archive"
      mu4e-get-mail-command "mbsync -a"
      ;mu4e-update-interval 900
      mu4e-view-show-images t
      mu4e-view-show-addresses t
      mu4e-compose-dont-reply-to-self t
      mu4e-user-mail-address-list '("jon.reeve@gmail.com" "jonathan.reeve@columbia.edu" "jpr2152@columbia.edu"))

;; Bookmarks
(setq mu4e-bookmarks
      `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
        ("date:7d..now NOT flag:trashed AND NOT flag:replied" "Last 7 days unreplied" ?w)
        ("maildir:/columbia/Inbox NOT flag:trashed AND NOT flag:replied" "Columbia" ?c)
        ("maildir:/gmail/Inbox NOT flag:trashed AND NOT flag:replied" "Gmail" ?g)
        ("maildir:/gmail/Lists NOT flag:trashed AND NOT flag:replied" "Lists" ?l)))

;; Set browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")

;; RSS
(eval-after-load 'shr
  '(progn (setq shr-width -1)
          (defun shr-fill-text (text) text)
          (defun shr-fill-lines (start end) nil)
          (defun shr-fill-line () nil)))
(add-hook 'elfeed-show-mode-hook 'visual-line-mode)

;; Keybindings
(global-set-key (kbd "<f2>") 'org-agenda-list)
(global-set-key (kbd "<f3>") 'org-todo-list)
(global-set-key (kbd "<f4>") 'mu4e)
(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "C-h") nil)
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-motion-state-map (kbd "C-e") nil)
  (define-key evil-normal-state-map (kbd "C-i") nil)
)
(global-unset-key (kbd "C-h"))
(global-unset-key (kbd "C-n"))
(global-unset-key (kbd "C-e"))
(global-unset-key (kbd "C-i"))
(global-set-key (kbd "C-h") 'evil-window-left)
(global-set-key (kbd "C-n") 'evil-window-down)
(global-set-key (kbd "C-e") 'evil-window-up)
(global-set-key (kbd "C-i") 'evil-window-right)
