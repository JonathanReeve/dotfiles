;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Enables Nixos-installed packages to be loaded
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; Set location of custom.el
(setq custom-file "~/.emacs.d/custom.el")

(setq doom-font (font-spec :family "Fira Code" :size 14))

(setq vc-follow-symlinks t) ;; Always follow symlinks.

;; Get system notifications through libnotify
(setq alert-default-style 'libnotify)

(after! eshell
  (when (and (executable-find "fish")
             (require 'fish-completion nil t))
    (global-fish-completion-mode)))

;; Don't prompt when opening journal or other large files
;(setq large-file-warning-threshold 20000000)

;; Default spelling dictionary is English
(setq ispell-dictionary "en")

;; Get system notifications through libnotify
(setq alert-default-style 'libnotify)

;; Bibliography
(setq! org-ref-notes-directory "")
(setq! +biblio-pdf-library-dir "~/Dokumentujo/Papers/"
       +biblio-default-bibliography-files '("~/Dokumentujo/Papers/library.bib")
       +biblio-notes-path "~/Dokumentujo/Org/Roam/")

;; Org Mode
(after! org
  (org-indent-mode)
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
  (map! :after org
        :map org-mode-map
        :localleader
        (:prefix-map ("n" . "notes")
         :prefix ("r" . "org-roam")
         "f" #'org-roam-node-find
         "g" #'org-roam-graph
         "m" #'org-roam
         "t" #'org-roam-tag-add
         "T" #'org-roam-tag-delete))

  (setq org-roam-directory "~/Dokumentujo/Org/Roam")
  (setq org-roam-dailies-directory "Daily/")
  (setq org-roam-db-location "~/Dokumentujo/Org/Roam/org-roam.db")
  ;; Hide the mode line in the org-roam buffer, since it serves no purpose. This
  ;; makes it easier to distinguish from other org buffers.
  ;; (add-hook 'org-roam-buffer-prepare-hook #'hide-mode-line-mode)


  ;; Automatically assign the tag Project for project notes
  ;; https://app.getpocket.com/read/3231443951
  (require 'vulpea)
  (defun vulpea-project-p ()
    "Return non-nil if current buffer has any todo entry.

  TODO entries marked as done are ignored, meaning the this
  function returns nil if current buffer contains only completed
  tasks."
    (seq-find                                 ; (3)
     (lambda (type)
       (eq type 'todo))
     (org-element-map                         ; (2)
         (org-element-parse-buffer 'headline) ; (1)
         'headline
       (lambda (h)
         (org-element-property :todo-type h)))))

  (defun vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-project-p)
              (setq tags (cons "project" tags))
            (setq tags (remove "project" tags)))
          (unless (eq original-tags tags)
            (apply #'vulpea-buffer-tags-set (seq-uniq tags)))))))

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun vulpea-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"project\"%"))]))))

  (defun vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (delete-dups (append org-agenda-files (vulpea-project-files)))))

  (add-hook 'find-file-hook #'vulpea-project-update-tag)
  (add-hook 'before-save-hook #'vulpea-project-update-tag)
  (advice-add 'org-agenda :before #'vulpea-agenda-files-update)

  ;; Org-roam-server
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)

  ;; Since the org module lazy loads org-protocol (waits until an org URL is
  ;; detected), we can safely chain `org-roam-protocol' to it.
  ;; (use-package! org-roam-protocol :after org-protocol)

  ;; Allow for "letter" class. This allows me to write subtrees in Org
  ;; and then later export them to Letter-class LaTeX-generated PDFs.
  ;; This is useful for drafting cover letters and the like.
  ;; (add-to-list 'org-latex-classes
  ;;              '("letter"
  ;;                "\\documentclass{letter}"
  ;;                ("\\section{%s}" . "\\section*{%s}")
  ;;                ("\\subsection{%s}" . "\\subsection*{%s}")
  ;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; I might not need this.
  ;; (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

  ;; Org-projectile stuff
  ;; (require 'org-projectile)
  ;; (setq org-projectile-projects-file
  ;;       "/your/path/to/an/org/file/for/storing/project/todos.org")
  ;; (push (org-projectile-project-todo-entry) org-capture-templates)
  ;; (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  ;; (global-set-key (kbd "C-c c") 'org-capture)
  ;; (global-set-key (kbd "C-c n p") 'org-projectile-project-todo-completing-read)
)

;;(setq org-agenda-window-setup 'only-window)
;; Prose linting
;; (require 'flycheck-vale)
;; (flycheck-vale-setup)

;; Open PDFs with system viewer
;; (delete '("\\.pdf\\'" . default) org-file-apps)
;; (add-to-list 'org-file-apps '("\\.pdf\\'" . system))

;; Markdown
(add-hook 'markdown-mode 'visual-line-mode)

;; Mail
(after! mu4e
  (require 'org-mu4e)
  ;; Use password-store as authentication source
  (require 'auth-source-pass)
  (auth-source-pass-enable)
  (setq auth-sources '(password-store))
  (set-email-account! "gmail"
                      '((mu4e-sent-folder   . "/gmail/[Gmail]/Sent Mail")
                        (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
                        (smtpmail-smtp-user . "jon.reeve")
                        (smtpmail-smtp-server . "smtp.gmail.com")
                        (smtpmail-smtp-service . 587)
                        (smtpmail-stream-type . starttls)
                        (user-mail-address  . "jon.reeve@gmail.com")
                        (mu4e-compose-signature . "--\nJonathan Reeve\nhttps://jonreeve.com"))
                      t)
  (set-email-account! "columbia"
                      '((mu4e-sent-folder   . "/columbia/[Gmail]/Sent Mail")
                        (mu4e-drafts-folder . "/columbia/[Gmail]/Drafts")
                        (smtpmail-smtp-user . "jpr2152@columbia.edu")
                        (user-mail-address  . "jpr2152@columbia.edu")
                        (smtpmail-smtp-server . "smtp.gmail.com")
                        (smtpmail-smtp-service . 587)
                        (smtpmail-stream-type . starttls)
                        (mu4e-compose-signature . "--\nJonathan Reeve\nPhD Candidate, Department of English and Comparative Literature\nhttps://jonreeve.com"))
                      t)
  (set-email-account! "personal"
                      '((mu4e-sent-folder   . "/personal/Sent")
                        (mu4e-drafts-folder . "/personal/Drafts")
                        (smtpmail-smtp-user . "jonathan@jonreeve.com")
                        (user-mail-address  . "jonathan@jonreeve.com")
                        (smtpmail-smtp-server . "mail.privateemail.com")
                        (smtpmail-smtp-service . 587)
                        (smtpmail-stream-type . starttls)
                        (mu4e-compose-signature . "--\nJonathan Reeve\nhttps://jonreeve.com"))
                      t)
  (set-email-account! "protonmail"
                      '((mu4e-sent-folder   . "/protonmail/Sent")
                        (mu4e-drafts-folder . "/protonmail/Drafts")
                        (smtpmail-smtp-user . "jonathan@jonreeve.com")
                        (user-mail-address  . "jonathan@jonreeve.com")
                        (smtpmail-smtp-server . "127.0.0.1")
                        (smtpmail-smtp-service . 1025)
                        (smtpmail-stream-type . starttls)
                        (mu4e-compose-signature . "--\nJonathan Reeve\nhttps://jonreeve.com"))
                      t)
  (setq message-send-mail-function 'smtpmail-send-it
        )
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
          ("maildir:/columbia/Inbox OR maildir:/gmail/Inbox OR maildir:/personal/Inbox OR maildir:/protonmail/Inbox NOT flag:trashed" "All" ?a)
          ("maildir:/gmail/Inbox NOT flag:trashed AND NOT flag:replied" "Gmail" ?g)
          ("maildir:/gmail/Lists OR maildir:/protonmail/Lists NOT flag:trashed AND NOT flag:replied" "Lists" ?l)
          ("maildir:/personal/Inbox NOT flag:trashed AND NOT flag:replied" "Personal" ?p)
          ("maildir:/columbia/Homework NOT flag:trashed" "Homework" ?h)
          ))

)
  ;; (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
  ;; (setq mu4e-html2text-command "w3m -T text/html")


;; Set browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser")

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

;; Unbind QWERTY, bind Colemak
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

(map! :map bibtex-mode-map "")
(setq lsp-haskell-server-wrapper-function (lambda (argv)
                                            (append
                                             (append (list "nix-shell" "-I" "." "--command" )
                                                     (list (mapconcat 'identity argv " ")))
                                             (list (concat (lsp-haskell--get-root) "/shell.nix")))))

;; Epub
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                                           :height 1.6))
(add-hook 'nov-mode-hook 'my-nov-font-setup)

;; Toggle transparency
 (defun toggle-transparency ()
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(85 . 50) '(100 . 100)))))

;; Treat all themes as safe
(setq custom-safe-themes t)

;; Fancy splash image
;; (setq fancy-splash-image "/home/jon/Bildujo/typewriter1.jpg")

