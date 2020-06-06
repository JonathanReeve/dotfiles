;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq doom-font (font-spec :family "Fira Code" :size 14))

(setq vc-follow-symlinks t) ;; Always follow symlinks.

;; Get system notifications through libnotify
(setq alert-default-style 'libnotify)

;; Don't prompt when opening journal or other large files
;(setq large-file-warning-threshold 20000000)

;; Bibliography
(setq! org-ref-notes-directory "")
(setq! +biblio-pdf-library-dir "~/Dropbox/Papers/"
       +biblio-default-bibliography-files '("~/Dropbox/Papers/library.bib")
       +biblio-notes-path "~/Dropbox/Org/Projects/books.org")

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

(require 'auth-source-pass)
(auth-source-pass-enable)

;; Org Mode
(after! org
  (setq org-directory "~/Dropbox/Org"
        org-startup-indented t
        evil-org-key-theme '(textobjects navigation additional insert todo)
        org-default-priority ?C
        org-lowest-priority ?G
        org-duration-format 'h:mm
        diary-file "~/Dropbox/Org/diary"
        org-agenda-include-diary t
        org-agenda-files (list "~/Dropbox/Org/Projects/")
        org-projectile-file "~/Dropbox/Org/Projects/todo.org"
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE" "CANCELED"))
        org-todo-keywords-for-agenda '((sequence "TODO" "WAITING" "|" "DONE" "CANCELED"))
        )
  (setq org-refile-targets '((nil :maxlevel . 9)
                              (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (setq org-refile-use-outline-path t)                  ; Show full paths for refiling

  ;; Adapted from http://stackoverflow.com/a/12751732/584121
  ; (require 'org-protocol)
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
  (add-hook 'diary-mode-hook 'diary-fancy-display-mode)
  (defun org-journal-new-entry ()
    "Inserts header with inactive timestamp, hours and minutes.
     A custom journal helper function."
    (interactive)
    (org-insert-heading)
    (org-insert-time-stamp (current-time) t t))
  ;; Clock break time in pomodoro
  (setq org-pomodoro-clock-break t)
  (add-hook 'org-mode-hook 'visual-line-mode)


   ;; Org-brain
   (setq org-brain-path "~/Dropbox/Org/Brain")
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
;; Send email via Gmail:
(after! mu4e
  (set-email-account! "Gmail"
                      '((mu4e-sent-folder   . "/gmail/[Gmail]/.Sent Mail")
                        (mu4e-drafts-folder . "/gmail/[Gmail]/.Drafts")
                        (smtpmail-smtp-user . "jon.reeve")
                        (user-mail-address  . "jon.reeve@gmail.com")
                        (mu4e-compose-signature . "---\nJonathanReeve\njonreeve.com"))
                      t)
  (set-email-account! "Columbia"
                      '((mu4e-sent-folder   . "/columbia/[Gmail]/.Sent Mail")
                        (mu4e-drafts-folder . "/columbia/[Gmail]/.Drafts")
                        (smtpmail-smtp-user . "jpr2152@columbia.edu")
                        (user-mail-address  . "jpr2152@columbia.edu")
                        (mu4e-compose-signature . "---\nJonathan Reeve\nPhD Candidate, Department of English and Comparative Literature\nhttp://jonreeve.com"))
                      t)
  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        message-send-mail-function 'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.gmail.com")
  (setq mu4e-maildir "~/Mail"
        mu4e-trash-folder "/Trash"
        mu4e-refile-folder "/Archive"
        mu4e-view-show-addresses t
        mu4e-compose-dont-reply-to-self t
        mu4e-user-mail-address-list '("jon.reeve@gmail.com" "jonathan.reeve@columbia.edu" "jpr2152@columbia.edu"))
  (setq mu4e-bookmarks
        `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
          ("date:7d..now NOT flag:trashed AND NOT flag:replied" "Last 7 days unreplied" ?w)
          ("maildir:/columbia/Inbox NOT flag:trashed AND NOT flag:replied" "Columbia" ?c)
          ("maildir:/columbia/Inbox and maildir:/gmail/Indox NOT flag:trashed" "All" ?a)
          ("maildir:/gmail/Inbox NOT flag:trashed AND NOT flag:replied" "Gmail" ?g)
          ("maildir:/gmail/Lists NOT flag:trashed AND NOT flag:replied" "Lists" ?l)))

  )
  ;; (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
  ;; (setq mu4e-html2text-command "w3m -T text/html")

;; Better looking HTML mail

;; Set browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")

;; RSS
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

;; Custom Keybindings
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

(use-package! evil-colemak-basics :after evil)

(after! evil
  (global-evil-colemak-basics-mode)
  (define-key evil-normal-state-map (kbd "n") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "e") 'evil-previous-visual-line)
  )
