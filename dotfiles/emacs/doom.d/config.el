;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq doom-font (font-spec :family "Fira Code" :size 14))

(setq vc-follow-symlinks t) ;; Always follow symlinks.

;; Get system notifications through libnotify
(setq alert-default-style 'libnotify)

;; Don't prompt when opening journal or other large files
;(setq large-file-warning-threshold 20000000)


(after! org-ref
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
  )

;; Get system notifications through libnotify
(setq alert-default-style 'libnotify)

;; Bibliography
(setq! org-ref-notes-directory "")
(setq! +biblio-pdf-library-dir "~/Dokumentujo/Papers/"
       +biblio-default-bibliography-files '("~/Dokumentujo/Papers/library.bib")
       +biblio-notes-path "~/Dokumentujo/Org/Projects/books.org")


;; Org Mode
(after! org
  (setq org-directory "~/Dokumentujo/Org"
        org-startup-indented t
        org-startup-folded t
        evil-org-key-theme '(textobjects navigation additional insert todo)
        org-default-priority ?C
        org-lowest-priority ?G
        org-duration-format 'h:mm
        diary-file "~/Dokumentujo/Org/diary"
        org-agenda-include-diary t
        org-agenda-files (list "~/Dokumentujo/Org/Projects/")
        org-projectile-file "~/Dokumentujo/Org/Projects/todo.org"
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE" "CANCELED"))
        org-todo-keywords-for-agenda '((sequence "TODO" "WAITING" "|" "DONE" "CANCELED"))
        ;; Put state changes into the LOGBOOK drawer, to clean up a bit
        org-log-into-drawer t
        )
  (setq org-refile-targets '((nil :maxlevel . 9)
                              (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (setq org-refile-use-outline-path t)                  ; Show full paths for refiling

  ;; Adapted from http://stackoverflow.com/a/12751732/584121
  ; (require 'org-protocol)
  (setq org-protocol-default-template-key "l")
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "/home/jon/Dokumentujo/Org/notes.org" "Tasks")
            "* TODO %?  %i\n  %a")
          ("m" "Movie" entry (file+headline "/home/jon/Dokumentujo/Org/Brain/movies.org" "To Watch")
            "* %a\n %?\n %i")
          ("l" "Link" entry (file+olp "/home/jon/Dokumentujo/Org/notes.org" "Web Links")
            "* %a\n %?\n %i")
          ("s" "Schedule" entry (file "/home/jon/Dokumentujo/Org/Projects/schedule.org")
            "* %?\n :PROPERTIES:\n :LOCATION:\n :END:\n %a\n %i")
          ))
  (setq org-modules '(org-habit org-protocol))
  ;; Disable holidays. Is there an easier way of doing this?
  (setq holiday-christian-holidays nil
        holiday-islamic-holidays nil
        holiday-bahai-holidays nil
        holiday-oriental-holidays nil
        holiday-hebrew-holidays nil)
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (calendar-set-date-style 'iso)))

  (defun org-journal-new-entry ()
    "Inserts header with inactive timestamp, hours and minutes.
     A custom journal helper function."
    (interactive)
    (org-insert-heading)
    (org-insert-time-stamp (current-time) t t))
  ;; Clock break time in pomodoro
  (setq org-pomodoro-clock-break t)
  (add-hook 'org-mode-hook 'visual-line-mode)

  ;; Org-roam
  (setq org-roam-directory "~/Dokumentujo/Org/Roam")
)

;;(setq org-agenda-window-setup 'only-window)
;; Prose linting
;; (require 'flycheck-vale)
;; (flycheck-vale-setup)

;; Open PDFs with system viewer
;; (delete '("\\.pdf\\'" . default) org-file-apps)
;; (add-to-list 'org-file-apps '("\\.pdf\\'" . system))

;; Org-brain

;; Markdown
(add-hook 'markdown-mode 'visual-line-mode)


;; Mail
(after! mu4e
  (require 'org-mu4e)
  ;; Use password-store as authentication source
  (require 'auth-source-pass)
  (auth-source-pass-enable)
  (setq auth-sources '(password-store))
  (set-email-account! "Gmail"
                      '((mu4e-sent-folder   . "/gmail/[Gmail]/.Sent Mail")
                        (mu4e-drafts-folder . "/gmail/[Gmail]/.Drafts")
                        (smtpmail-smtp-user . "jon.reeve")
                        (smtpmail-smtp-server "smtp.gmail.com")
                        (smtpmail-smtp-service 587)
                        (user-mail-address  . "jon.reeve@gmail.com")
                        (mu4e-compose-signature . "---\nJonathanReeve\njonreeve.com"))
                      t)
  (set-email-account! "Columbia"
                      '((mu4e-sent-folder   . "/columbia/[Gmail]/.Sent Mail")
                        (mu4e-drafts-folder . "/columbia/[Gmail]/.Drafts")
                        (smtpmail-smtp-user . "jpr2152@columbia.edu")
                        (user-mail-address  . "jpr2152@columbia.edu")
                        (smtpmail-smtp-server "smtp.gmail.com")
                        (smtpmail-smtp-service 587)
                        (mu4e-compose-signature . "---\nJonathan Reeve\nPhD Candidate, Department of English and Comparative Literature\nhttp://jonreeve.com"))
                      t)
  (set-email-account! "Personal"
                      '((mu4e-sent-folder   . "/personal/Sent")
                        (mu4e-drafts-folder . "/personal/Drafts")
                        (smtpmail-smtp-user . "jonathan@jonreeve.com")
                        (user-mail-address  . "jonathan@jonreeve.com")
                        (smtpmail-smtp-server "mail.privateemail.com")
                        (smtpmail-smtp-service 465)
                        (smtpmail-stream-type 'ssl)
                        (mu4e-compose-signature . "---\nJonathan Reeve\nhttp://jonreeve.com"))
                      t)
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq mu4e-maildir "~/Mail"
        mu4e-trash-folder "/Trash"
        mu4e-refile-folder "/Archive"
        mu4e-view-show-addresses t
        mu4e-attachment-dir "~/Elŝutujo"
        mu4e-compose-dont-reply-to-self t
        mu4e-user-mail-address-list '("jon.reeve@gmail.com" "jonathan.reeve@columbia.edu" "jpr2152@columbia.edu"))
  (setq mu4e-bookmarks
        `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
          ("date:7d..now NOT flag:trashed AND NOT flag:replied" "Last 7 days unreplied" ?w)
          ("maildir:/columbia/Inbox NOT flag:trashed AND NOT flag:replied" "Columbia" ?c)
          ("maildir:/columbia/Inbox and maildir:/gmail/Indox NOT flag:trashed" "All" ?a)
          ("maildir:/gmail/Inbox NOT flag:trashed AND NOT flag:replied" "Gmail" ?g)
          ("maildir:/gmail/Lists NOT flag:trashed AND NOT flag:replied" "Lists" ?l)
          ("maildir:/personal/Inbox NOT flag:trashed AND NOT flag:replied" "Personal" ?p)
          ))

)
  ;; (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
  ;; (setq mu4e-html2text-command "w3m -T text/html")


;; Set browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/bin/qutebrowser")

;; Better looking HTML mail
(after! shr
  (setq shr-color-visible-luminance-min 80)
  (setq shr-use-colors nil)
  (setq shr-use-fonts nil)
  (define-advice mm-shr (:around (oldfn &rest handle) delete-trailing-whitespace)
    "Delete leading and trailing whitespace in Gnus article buffer."
    (if (derived-mode-p 'gnus-article-mode)
        (save-restriction
          (narrow-to-region (point) (point))
          (apply oldfn handle)
          (delete-trailing-whitespace))
      (apply oldfn handle)))
  '(progn (setq shr-width -1)
          (defun shr-fill-text (text) text)
          (defun shr-fill-lines (start end) nil)
          (defun shr-fill-line () nil)))

(add-hook 'elfeed-show-mode-hook 'visual-line-mode)

(map! :n "l" #'evil-insert
      :n "L" #'evil-insert-line
      :nvm "n" #'evil-next-visual-line
      :nvm "gn" #'evil-next-line
      :nvm "gN" #'evil-next-visual-line
      :nvm "e" #'evil-previous-visual-line
      :nvm "ge" #'evil-previous-line
      :nvm "i" #'evil-forward-char
      :nvm "j" #'evil-fordard-word-end
      :nvm "J" #'evil-forward-WORD-end
      :nvm "gj" #'evil-backward-word-end
      :nvm "gJ" #'evil-backward-WORD-end
      :nvm "k" #'evil-ex-search-next
      :nvm "K" #'evil-ex-search-previous
      :nvm "f" #'evil-snipe-f
      :nvm "F" #'evil-snipe-F
      :nvm "t" #'evil-snipe-t
      :nvm "T" #'evil-snipe-T
      :nvm "s" #'evil-snipe-s
      :nvm "S" #'evil-snipe-S
      :nv "u" #'undo-tree-undo
      :nv "N" #'evil-join
      :nv "gN" #'evil-join-whitespace
      :g "C-h" #'evil-window-left
      :g "C-n" #'evil-window-down
      :g "C-e" #'evil-window-up
      :g "C-i" #'evil-window-right
      :g "<f2>" #'org-agenda-list
      :g "<f3>" #'org-todo-list
      )

(map! :map evil-treemacs-state-map "n" 'treemacs-next-line
                                   "e" 'treemacs-previous-line)

(setq lsp-haskell-server-wrapper-function (lambda (argv)
                                            (append
                                             (append (list "nix-shell" "-I" "." "--command" )
                                                     (list (mapconcat 'identity argv " ")))
                                             (list (concat (lsp-haskell--get-root) "/shell.nix")))))
