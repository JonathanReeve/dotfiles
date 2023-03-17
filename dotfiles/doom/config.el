;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Jonathan Reeve"
      user-mail-address "jonathan@jonreeve.com")

;; Enables Nixos-installed packages to be loaded
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; Set location of custom.el
(setq custom-file "~/.config/emacs/custom.el")


;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
;; (setq doom-font (font-spec :family "Fantasque Sans Mono" :size 18))
(setq doom-themes-treemacs-enable-variable-pitch 'nil)

(setq doom-modeline-buffer-file-name-style 'truncate-with-project)
;;(setq doom-font (font-spec :family "Fira Code Nerd Font" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))

(setq vc-follow-symlinks t) ;; Always follow symlinks.

;; Get system notifications through libnotify
(setq alert-default-style 'libnotify)

;; Don't prompt when opening journal or other large files
;(setq large-file-warning-threshold 20000000)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/Org/")

;; Default spelling dictionary is English
(setq ispell-dictionary "english")

(after! spell-fu
  (add-hook 'spell-fu-mode-hook
            (lambda ()
              (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
              (spell-fu-dictionary-add
               (spell-fu-get-personal-dictionary "en-personal" "/Users/jon/Dotfiles/scripts/aspell.en.pws")))))

;; Bibliography

;; Citar
;; See https://github.com/hlissner/doom-emacs/blob/4612b39695405f7238dd3da0d4fd6d3a5cdd93d6/modules/tools/biblio/README.org
(setq! citar-bibliography '("~/Documents/Papers/library.bib" "~/Documents/Papers/library2.bib")
       citar-library-paths '("~/Documents/Papers/")
       citar-notes-paths '("~/Documents/Org/Roam/"))

(setq! bibtex-completion-bibliography '("~/Documents/Papers/library.bib" "~/Documents/Papers/library2.bib")
       bibtex-completion-notes-path "~/Documents/Org/Roam/"
       bibtex-completion-library-path "~/Documents/Papers/")

;; Org Mode
(after! org
  (org-indent-mode)
  (setq org-directory "~/Documents/Org"
        org-startup-indented t
        org-startup-folded t
        evil-org-key-theme '(textobjects navigation additional insert todo)
        org-default-priority ?C
        org-lowest-priority ?G
        org-duration-format 'h:mm
        org-agenda-files (list "~/Documents/Org")
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
        '(("t" "Todo" entry (file+headline "/Users/jon/Documents/Org/inbox.org" "Tasks")
            "* TODO %?  %i\n  %a")
          ("m" "Meeting" entry (file+headline "~/Documents/Org/meetings.org" "Meetings")
            "** %? %T \n%a\n" :clock-in t)
          ("r" "Radar" entry (file+headline "~/Documents/Org/todo.org" "Radars")
            "** TODO %c\n%?")
          ("c" "Code" entry (file+headline "~/Documents/Org/inbox.org" "Code")
            "** %A\n%?")
          ("o" "Contact" entry (file+headline "~/Documents/Org/contacts.org" "Contacts")
            "** %? \n%a\n")
          ("l" "Link" entry (file+olp "/Users/jon/Documents/Org/inbox.org" "Web Links")
            "* %a\n %?\n %i")
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

  (setq org-roam-directory "~/Documents/Org/Roam")
  (setq org-roam-dailies-directory "Daily/")
  (setq org-roam-db-location "~/Documents/Org/Roam/org-roam.db")

  (setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
"#+title: %<%Y-%m-%d>

#+BEGIN: clocktable :scope agenda :maxlevel 2 :step day :fileskip0 true :tstart \"%<%Y-%m-%d>\" :tend \"%<%Y-%m-%d>\"
#+END: "))))

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("c" "contact" plain "** ${title}\n :PROPERTIES:\n :ID: %(org-id-uuid)\n :END:\n%u\n"
           :target (file+olp "contacts.org" ("Contacts"))
           :unnarrowed t)
        ))
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%?" :target
           (file+head "${slug}.org" "#+title: ${title}") :unnarrowed t)
          ("m" "movie" plain "** ${title}\n :PROPERTIES:\n :ID: %(org-id-uuid)\n :RATING:\n :WIKIDATA: ${ref}\n :END:\n%u\n"
           :target (file+olp "movies.org" ("watched")))
          )
        )


  (setq org-clock-idle-time 15)
  (setq org-clock-auto-clockout t)
  (setq org-clock-auto-clockout-timer 20)

  (require 'org-roam-bibtex)
  (use-package! org-roam-bibtex
    :when (featurep! :lang org +roam2)
    :after org
    :preface
    ;; if the user has not set a template mechanism set a reasonable one of them
    ;; The package already tests for nil itself so we define a dummy tester
    (defvar orb-preformat-keywords
      '("title" "url" "file" "author-or-editor" "keywords" "citekey" "pdf"))
    ;;:hook (org-roam-mode . org-roam-bibtex-mode)
    :custom
    (orb-note-actions-interface 'default)
    :config
    (setq orb-insert-interface 'generic)
    ;; (setq orb-roam-ref-format 'org-ref-v2)
    (setq orb-process-file-keyword t
          orb-file-field-extensions '("pdf"))

    (add-to-list 'org-roam-capture-templates
                 '("b" "Bibliography note" plain
                   "%?
- keywords :: %^{keywords}
- related ::

* %^{title}
:PROPERTIES:
:Custom_ID: %^{citekey}
:URL: %^{url}
:AUTHOR: %^{author-or-editor}
:NOTER_DOCUMENT: %^{file}
:NOTER_PAGE:
:END:\n\n"
                   :if-new (file+head "${citekey}.org" ":PROPERTIES:
:END:
#+TITLE: ${citekey}: ${title}\n")
                   :unnarrowed t))
    (require 'org-ref))
  (org-roam-bibtex-mode)

  (setq citar-templates
        '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
         (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
         (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
         (note . "#+title: ${author editor}, ${title}")))

  (setq citar-symbols
        `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (setq citar-symbol-separator "  ")

  (setq citar-file-open-note-function 'orb-citar-edit-note)
  ;; (setq citar-file-open-note-function 'citar-file-open-notes-default-org)

  ;; Configure org-roam buffer display.
  ;; See https://www.orgroam.com/manual.html#Navigating-the-Org_002droam-Buffer
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))

  (use-package! websocket
    :after org-roam)

  (use-package! org-roam-ui
    :after org-roam ;; or :after org
    ;; :hook
    ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
    ;;         a hookable mode anymore, you're advised to pick something yourself
    ;;         if you don't care about startup time, use
    ;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start nil)
    )

  ;; Hide the mode line in the org-roam buffer, since it serves no purpose. This
  ;; makes it easier to distinguish from other org buffers.
  ;; (add-hook 'org-roam-buffer-prepare-hook #'hide-mode-line-mode)

  ;; Automatically assign the tag Project for project notes
  ;; Code: https://gist.github.com/d12frosted/a60e8ccb9aceba031af243dff0d19b2e
  ;; Original blog post: https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html

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

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

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

;; (add-hook 'find-file-hook #'vulpea-project-update-tag)
(add-hook 'before-save-hook #'vulpea-project-update-tag)
(add-hook 'org-agenda-mode-hook #'vulpea-agenda-files-update)
;; (remove-hook 'org-agenda-mode-hook #'vulpea-agenda-files-update)

;; functions borrowed from `vulpea' library
;; https://github.com/d12frosted/vulpea/blob/6a735c34f1f64e1f70da77989e9ce8da7864e5ff/vulpea-buffer.el

(defun vulpea-buffer-tags-get ()
  "Return filetags value in current buffer."
  (vulpea-buffer-prop-get-list "filetags" "[ :]"))

(defun vulpea-buffer-tags-set (&rest tags)
  "Set TAGS in current buffer.
If filetags value is already set, replace it."
  (if tags
      (vulpea-buffer-prop-set
       "filetags" (concat ":" (string-join tags ":") ":"))
    (vulpea-buffer-prop-remove "filetags")))

(defun vulpea-buffer-tags-add (tag)
  "Add a TAG to filetags in current buffer."
  (let* ((tags (vulpea-buffer-tags-get))
         (tags (append tags (list tag))))
    (apply #'vulpea-buffer-tags-set tags)))

(defun vulpea-buffer-tags-remove (tag)
  "Remove a TAG from filetags in current buffer."
  (let* ((tags (vulpea-buffer-tags-get))
         (tags (delete tag tags)))
    (apply #'vulpea-buffer-tags-set tags)))

(defun vulpea-buffer-prop-set (name value)
  "Set a file property called NAME to VALUE in buffer file.
If the property is already set, replace its value."
  (setq name (downcase name))
  (org-with-point-at 1
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                             (point-max) t)
          (replace-match (concat "#+" name ": " value) 'fixedcase)
        (while (and (not (eobp))
                    (looking-at "^[#:]"))
          (if (save-excursion (end-of-line) (eobp))
              (progn
                (end-of-line)
                (insert "\n"))
            (forward-line)
            (beginning-of-line)))
        (insert "#+" name ": " value "\n")))))

(defun vulpea-buffer-prop-set-list (name values &optional separators)
  "Set a file property called NAME to VALUES in current buffer.
VALUES are quoted and combined into single string using
`combine-and-quote-strings'.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t.
If the property is already set, replace its value."
  (vulpea-buffer-prop-set
   name (combine-and-quote-strings values separators)))

(defun vulpea-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                             (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

(defun vulpea-buffer-prop-get-list (name &optional separators)
  "Get a buffer property NAME as a list using SEPARATORS.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t."
  (let ((value (vulpea-buffer-prop-get name)))
    (when (and value (not (string-empty-p value)))
      (split-string-and-unquote value separators))))

  ;; (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

  ;; Org-projectile stuff
  ;; (require 'org-projectile)
  ;; (setq org-projectile-projects-file
  ;;       "/your/path/to/an/org/file/for/storing/project/todos.org")
  ;; (push (org-projectile-project-todo-entry) org-capture-templates)
  ;; (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  ;; (global-set-key (kbd "C-c c") 'org-capture)
  ;; (global-set-key (kbd "C-c n p") 'org-projectile-project-todo-completing-read)
  (setq org-link-abbrev-alist
      '(("wikidata"        . "https://www.wikidata.org/wiki/")))

  (org-link-set-parameters "rdar" :follow #'rdar-open)
  (org-link-set-parameters "phantom" :follow #'phantom-open)

  (defun rdar-open (path _) ; "_" here is the universal prefix argument, you can define different behavior if you like
    (browse-url (concat "rdar:" path)))

  (defun phantom-open (path _) ; "_" here is the universal prefix argument, you can define different behavior if you like
    (browse-url (concat "phantom:" path)))

  ;; Disable editing source code in dedicated buffer
  ;; https://emacs.stackexchange.com/questions/73986/how-do-i-stop-org-babel-from-trying-to-edit-a-source-block-in-a-dedicated-buffer/73988#73988
  (defun org-edit-src-code nil)

  ;; Org-mac-protocol
  ;; (require 'org-bibtex)
  ;; (require 'org-mac-protocol)
  ;; (setq org-remember-templates
  ;;     '((("AppleScript remember" ?y "* %:shortdesc\n  %:initial\n   Source: %u, %c\n\n  %?" (concat org-directory "inbox.org") "Remember"))
  ;;       (("AppleScript note" ?z "* %?\n\n  Date: %u\n" (concat org-directory "inbox.org") "Notes")))
  ;; )
) ;; End of Org block

;; (use-package! org-clock-reminder
;;   :config
;;   (org-clock-reminder-activate)
;;   (setq org-clock-reminder-remind-inactivity 't))

;;(setq org-agenda-window-setup 'only-window)

;; Prose linting
;; (require 'flycheck-vale)
;; (flycheck-vale-setup)

;; Markdown
(add-hook 'markdown-mode 'visual-line-mode)

;; Set browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "open")

;; Unbind QWERTY, bind Colemak
(map! :n "l" #'evil-insert
      :n "L" #'evil-insert-line
      :nvm "n" #'evil-next-visual-line
      :nvm "gn" #'evil-next-line
      :nvm "gN" #'evil-next-visual-line
      :nvm "e" #'evil-previous-visual-line
      :nvm "ge" #'evil-previous-line
      :nvm "i" #'evil-forward-char
      :nvm "j" #'evil-forward-word-end
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

(map! :map evil-window-map "n" #'evil-window-down
                           "N" 'evil-window-move-very-bottom
                           "e" 'evil-window-up
                           "E" 'evil-window-move-very-top
                           "i" 'evil-window-right
                           "I" 'evil-window-move-far-right
                           "j" 'evil-window-new)

(map! :n "SPC w c" 'evil-window-new)

;; Bind stuff
(map! :after pdf-tools :map pdf-view-mode-map :n "C-i" 'org-noter-insert-precise-note
                             :n "C-n" 'pdf-view-next-page
                             :n "C-e" 'pdf-view-previous-page
                             :n "n"   'pdf-view-scroll-up-or-next-page
                             :n "e"   'pdf-view-scroll-down-or-previous-page)

(map! :after ranger :map ranger-normal-mode-map
      :nvm "h" 'ranger-up-directory
      :nvm "n" 'ranger-next-file
      :nvm "e" 'ranger-prev-file
      :nvm "i" 'ranger-find-file
      :nvm "N" 'ranger-half-page-down
      :nvm "E" 'ranger-half-page-up
      )

;; Epub
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                                           :height 1.4))
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
;; (setq fancy-splash-image "/Users/jon/Bildujo/typewriter1.jpg")

;; Stop autocompleting parentheses and quotation marks
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(after! org-cite
  (require 'oc-csl-activate)
  (setq org-cite-activate-processor 'csl-activate)
  )

;; (add-to-list 'auto-mode-alist '("\\.cljs\\.hl\\'" . clojurescript-mode)

(defun rename-pdf ()
  " Rename the most recently modified PDF in the /tmp dir with the latest bibtex key. "
  (interactive)
  (setq most-recent-pdf (string-trim-right (shell-command-to-string "ls -t /tmp/*.pdf | head -1")))
  (setq dest-pdf-filename (string-trim-right (concat bibtex-completion-library-path (bibtex-completion-get-key-bibtex) ".pdf")))
  (if (yes-or-no-p (concat "Rename " most-recent-pdf " to " dest-pdf-filename "?"))
  (rename-file most-recent-pdf dest-pdf-filename)
  (message "Aborted.")
  ))

;; (use-package! notebook-mode)

;; (use-package! evil-colemak-basics
;;   :after evil
;;   :config
;;   (global-evil-colemak-basics-mode) ; Enable colemak rebinds
;;   )

;; Workaround; see https://github.com/nnicandro/emacs-jupyter/issues/380#issuecomment-1014026589
(after! ob-jupyter
  (defun jupyter-ansi-color-apply-on-region (begin end)
    (ansi-color-apply-on-region begin end t))
  )

;; Encryption
(require 'epa-file)
(epa-file-enable)

;; (setq system-uses-terminfo nil)

(set-locale-environment "eo.utf-8")

;; Scala
;; (add-to-list '+org-babel-mode-alist '(scala . ammonite))

;; Workaround for Treemacs issue; see https://github.com/doomemacs/doomemacs/issues/7126
;; Can remove when this PR is merged: https://github.com/doomemacs/doomemacs/pull/7134
(set-popup-rule! "^ ?\\*Treemacs" :ignore t)
