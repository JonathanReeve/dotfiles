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
;; (setq doom-font (font-spec :family "Monaspace Argon" :size 12)
;;       doom-variable-pitch-font (font-spec :family "Helvetica" :size 13))
;;
;; (setq doom-font (font-spec :family "Iosevka Comfy" :size 12)
;;       doom-variable-pitch-font (font-spec :family "Helvetica" :size 13))

(setq doom-font (font-spec :family "Agave" :size 14)
      doom-variable-pitch-font (font-spec :family "Helvetica" :size 14))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
;; (setq doom-font (font-spec :family "Fantasque Sans Mono" :size 18))
;; (setq doom-themes-treemacs-enable-variable-pitch 'nil)

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
       citar-notes-paths '("~/Documents/Org/Roam/shared"))

(setq! bibtex-completion-bibliography '("~/Documents/Papers/library.bib" "~/Documents/Papers/library2.bib")
       bibtex-completion-notes-path "~/Documents/Org/Roam/"
       bibtex-completion-library-path '("~/Documents/Papers/" "~/Documents/Org/Roam/shared/papers/"))

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
        org-agenda-dim-blocked-tasks nil
        org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE(!)" "CANCELED"))
        org-todo-keywords-for-agenda '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d!)" "CANCELED(c)"))
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
          ("l" "Link" entry (file+olp "/Users/jon/Documents/Org/inbox.org" "Web Links")
            "* %a\n %?\n %i")
          ("j" "Journal" entry (file+olp+datetree "/Users/jon/Documents/Org/journal.org")
           "* %?" :tree-type week)
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
          ("a" "shared" plain "%?" :target
           (file+head "shared/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("c" "contact" plain "** ${title}\n :PROPERTIES:\n :ID: %(org-id-uuid)\n :ADIR: \n :END:\n%u\n"
           :target (file+olp "contacts.org" ("Contacts"))
           :unnarrowed t)
          ("s" "dataset" plain "** ${title}\n :PROPERTIES:\n :ID: %(org-id-uuid)\n :LOCATION: \n :END:\n%u\n*** Schema"
           :target (file+head "datasets.org" "")
           :unnarrowed t
           )
          ("j" "job" plain "* ${title}\n :PROPERTIES:\n :ID: %(org-id-uuid)\n :CODE: \n :END:\n%u\ninputs:\noutputs:"
           :target (file+head "jobs.org" "")
           :unnarrowed t
           )
          ("r" "cluster" plain "* ${title}\n :PROPERTIES:\n :ID: %(org-id-uuid)\n :ADDRESS:\n :END:\n%u"
           :target (file+head "servers.org" "")
           :unnarrowed t
           )
          ("e" "service" plain "* ${title}\n :PROPERTIES:\n :ID: %(org-id-uuid)\n :CODE:\n :END:\ninputs:\noutputs: \n%u"
           :target (file+head "services.org" "")
           :unnarrowed t
           )
          ("p" "repo" plain "* ${title}\n :PROPERTIES:\n :ID: %(org-id-uuid)\n :LOCAL:\n :REMOTE:\n :END:\n%u"
           :target (file+head "repos.org" "")
           :unnarrowed t
           )
          ("b" "literature note" plain
           "%?"
           :target
           (file+head
            "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/shared/${citar-citekey}.org"
            "#+title: ${citar-citekey} (${citar-date}). ${note-title}.
#+created: %U
#+last_modified: %U

 - keywords ::
 - related ::

* ${note-title}
  :PROPERTIES:
  :Custom_ID: ${citar-citekey}
  :URL: ${citar-url}
  :AUTHOR: ${citar-author}
  :NOTER_DOCUMENT: ${citar-file}
  :NOTER_PAGE:
  :END:\n
"
            )
           :unnarrowed t)
        ))
  (setq citar-org-roam-template-fields
      '((:citar-title "title")
        (:citar-author "author" "editor")
        (:citar-date "date" "year" "issued")
        (:citar-pages "pages")
        (:citar-file "file")
        (:citar-keywords "keywords")
        (:citar-url "url")
        (:citar-type "=type="))
        )
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%?" :target
           (file+head "${slug}.org" "#+title: ${title}") :unnarrowed t)
          ("m" "movie" plain "** ${title}\n :PROPERTIES:\n :ID: %(org-id-uuid)\n :RATING:\n :WIKIDATA: ${ref}\n :END:\n%u\n"
           :target (file+olp "movies.org" ("watched")))
          )
        )
  (setq citar-org-roam-note-title-template "${author editor} : ${title}")


  (setq org-clock-idle-time 15)
  (setq org-clock-auto-clockout t)
  (setq org-clock-auto-clockout-timer 20)

  (setq citar-templates
        '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
         (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
         (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
         (note . "#+title: ${author editor}, ${title}")))

  ;; (setq citar-symbols
  ;;       `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
  ;;         (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
  ;;         (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
  (setq citar-symbol-separator "  ")

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

   (use-package! citar-org-roam
        :after (citar org-roam)
        :config (citar-org-roam-mode)
                (setq citar-org-roam-capture-template-key "b")
        )

   (use-package! org-node
     :after org
     :config (org-node-cache-mode))

   (use-package! org-node-fakeroam :defer)


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

  (org-link-set-parameters "phantom" :follow #'phantom-open)
  (org-link-set-parameters "rdar" :follow #'rdar-open)
  (org-link-set-parameters "adir" :follow #'adir-open)

  (defun rdar-open (path _)
    (browse-url (concat "adir:" path)))

  (defun rdar-open (path _)
    (browse-url (concat "rdar:" path)))

  (defun phantom-open (path _)
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
  ;
  ;; (with-eval-after-load 'org (global-org-modern-mode))

  ;; Fancy tags
  ;; (require 'svg-tag-mode)

  ;; (setq svg-tag-tags
  ;;     '((":TODO:" . ((svg-tag-make "TODO" :face 'org-tag
  ;;                                  :radius 0 :inverse t :margin 0)))
  ;;       (":WAITING:" . ((svg-tag-make "WAITING" :face 'org-tag
  ;;                                  :radius 0 :inverse t :margin 0)))
  ;;       (":DONE:" . ((svg-tag-make "WAITING" :face 'org-tag
  ;;                                  :radius 0 :inverse t :margin 0)))
  ;;       (":CANCELED:" . ((svg-tag-make "WAITING" :face 'org-tag
  ;;                                  :radius 0 :inverse t :margin 0)))
  ;;       (":NOTE:" . ((svg-tag-make "NOTE" :face 'font-lock-comment-face
  ;;                                  :inverse nil :margin 0 :radius 0)))
  ;;       ("\([0-9a-zA-Z]\)" . ((lambda (tag)
  ;;                               (svg-tag-make tag :beg 1 :end -1 :radius 12))))
  ;;       ("\([0-9a-zA-Z][0-9a-zA-Z]\)" . ((lambda (tag)
  ;;                                          (svg-tag-make tag :beg 1 :end -1 :radius 8))))
  ;;       ("|[0-9a-zA-Z- ]+?|" . ((lambda (tag)
  ;;                                 (svg-tag-make tag :face 'font-lock-comment-face
  ;;                                               :margin 0 :beg 1 :end -1))))
  ;;       ;; Org tags
  ;;       (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
  ;;       (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

  ;;       ;; Task priority
  ;;       ("\\[#[A-Z]\\]" . ( (lambda (tag)
  ;;                             (svg-tag-make tag :face 'org-priority
  ;;                                           :beg 2 :end -1 :margin 0))))

  ;;       ;; Citation of the form [cite:@Knuth:1984]
  ;;       ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
  ;;                                         (svg-tag-make tag
  ;;                                                       :inverse t
  ;;                                                       :beg 7 :end -1
  ;;                                                       :crop-right t))))
  ;;       ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
  ;;                                               (svg-tag-make tag
  ;;                                                             :end -1
  ;;                                                             :crop-left t))))


  ;;       ;; Active date (with or without day name, with or without time)
  ;;       ;; (,(format "\\(<%s>\\)" date-re) .
  ;;       ;;  ((lambda (tag)
  ;;       ;;     (svg-tag-make tag :beg 1 :end -1 :margin 0))))
  ;;       ;; (,(format "\\(<%s \\)%s>" date-re day-time-re) .
  ;;       ;;  ((lambda (tag)
  ;;       ;;     (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
  ;;       ;; (,(format "<%s \\(%s>\\)" date-re day-time-re) .
  ;;       ;;  ((lambda (tag)
  ;;       ;;     (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

  ;;       ;; ;; Inactive date  (with or without day name, with or without time)
  ;;       ;;  (,(format "\\(\\[%s\\]\\)" date-re) .
  ;;       ;;   ((lambda (tag)
  ;;       ;;      (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
  ;;       ;;  (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
  ;;       ;;   ((lambda (tag)
  ;;       ;;      (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
  ;;       ;;  (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
  ;;       ;;   ((lambda (tag)
  ;;       ;;      (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))

  ;;       ;; ;; Progress
  ;;       ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
  ;;                                           (svg-progress-percent (substring tag 1 -2)))))
  ;;       ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
  ;;                                         (svg-progress-count (substring tag 1 -1)))))))
  ;;       (svg-tag-mode t)

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
  (setq most-recent-pdf (string-trim-right (shell-command-to-string "ls -t ~/Downloads/*.pdf | head -1")))
  (setq dest-pdf-filename (string-trim-right (concat (car bibtex-completion-library-path) (bibtex-completion-get-key-bibtex) ".pdf")))
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

(after! evil
  (setq org-fold-core-style 'overlays)
  (evil-select-search-module 'evil-search-module 'evil-search)
  )

;; Workaround; see https://github.com/nnicandro/emacs-jupyter/issues/380#issuecomment-1014026589
(after! ob-jupyter
  (defun jupyter-ansi-color-apply-on-region (begin end)
    (ansi-color-apply-on-region begin end t))
  )

;; Encryption
(require 'epa-file)
(epa-file-enable)

;; (setq system-uses-terminfo nil)
(after! vterm
        (setq vterm-shell "/etc/profiles/per-user/jon/bin/nu --config ~/.config/nushell/emacs-config.nu")
        (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
        (define-key vterm-mode-map (kbd "C-<escape>") #'vterm-send-next-key)
        )

;; (delete 'lsp-terraform lsp-client-packages)

;; (set-locale-environment "eo.utf-8")

;; Scala
;; (add-to-list '+org-babel-mode-alist '(scala . ammonite))

;; Workaround for Treemacs issue; see https://github.com/doomemacs/doomemacs/issues/7126
;; Can remove when this PR is merged: https://github.com/doomemacs/doomemacs/pull/7134
(set-popup-rule! "^ ?\\*Treemacs" :ignore t)

(delete-file "~/Library/Colors/Emacs.clr")

(require 'org-mac-link)
(defun org-mac-link-applescript-calendar-current-event()
  "AppleScript to get the current event from Calendar."
  (let ((result
	 (org-mac-link-do-applescript
	  (concat "
tell application \"Calendar\"
    set nowDate to current date
    set calendarEvents to (every event of every calendar whose start date ≥ nowDate) -- Retrieves all future events
    set sortedEvents to (sort calendarEvents by start date) -- Sorts events by start date
    repeat with theEvent in sortedEvents
        if start date of theEvent ≥ nowDate then -- Checks if the event occurs now or in the future
            set eventName to summary of theEvent
            set eventStartDate to start date of theEvent
            set eventEndDate to end date of theEvent
            display dialog \"Next Event: \" & eventName & return & \"Starts: \" & eventStartDate & return & \"Ends: \" & eventEndDate
            exit repeat
        end if
    end repeat
end tell
"))))
      (car (split-string result "[\r\n]+" t))))

(defun org-mac-link-calendar-current-event()
  "Get the link to the frontmost window of the Firefox.app."
  (interactive)
  (message "Applescript: Getting Calendar event...")
  (org-mac-link-paste-applescript-links (org-mac-link-applescript-calendar-current-event)))


(defun insert-first-rdar-string ()
  "Insert the first 'rdar' string (e.g., 'rdar://...') from the latest git log entry at point."
  (interactive)
  (let* ((git-log-output (shell-command-to-string "git log --oneline"))
         (first-line (car (split-string git-log-output "\n")))
         (rdar-string (when (string-match "\\(rdar://[0-9]+\\)" first-line)
                        (match-string 1 first-line))))
    (when rdar-string
      (insert rdar-string))))

;; Treemacs error. See https://github.com/emacs-lsp/lsp-mode/issues/4054
(add-to-list 'image-types 'svg)

;; Treemacs icons with LSP: https://github.com/emacs-lsp/lsp-treemacs/issues/89
;; (with-eval-after-load 'lsp-treemacs
;;           (doom-themes-treemacs-config))
;; (after! lsp-treemacs
;;   (load-library "doom-themes-ext-treemacs"))

;; EAF: Emacs Application Framework
;; (use-package! eaf
;;   :load-path "~/.elisp/emacs-application-framework"
;;   :init
;;   :custom
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser) ;; Make EAF Browser my default browser
;;   :config
;;   (defalias 'browse-web #'eaf-open-browser)

;;   (require 'eaf-file-manager)
;;   (require 'eaf-music-player)
;;   (require 'eaf-image-viewer)
;;   (require 'eaf-camera)
;;   (require 'eaf-demo)
;;   (require 'eaf-airshare)
;;   (require 'eaf-terminal)
;;   (require 'eaf-markdown-previewer)
;;   (require 'eaf-video-player)
;;   (require 'eaf-vue-demo)
;;   (require 'eaf-file-sender)
;;   (require 'eaf-pdf-viewer)
;;   (require 'eaf-mindmap)
;;   (require 'eaf-netease-cloud-music)
;;   (require 'eaf-jupyter)
;;   (require 'eaf-org-previewer)
;;   (require 'eaf-system-monitor)
;;   (require 'eaf-rss-reader)
;;   (require 'eaf-file-browser)
;;   (require 'eaf-browser)
;;   (require 'eaf-org)
;;   (require 'eaf-mail)
;;   (require 'eaf-git)
;;   (when (display-graphic-p)
;;     (require 'eaf-all-the-icons))

;;   (require 'eaf-evil)
;;   (define-key key-translation-map (kbd "SPC")
;;     (lambda (prompt)
;;       (if (derived-mode-p 'eaf-mode)
;;           (pcase eaf--buffer-app-name
;;             ("browser" (if  (string= (eaf-call-sync "call_function" eaf--buffer-id "is_focus") "True")
;;                            (kbd "SPC")
;;                          (kbd eaf-evil-leader-key)))
;;             ("pdf-viewer" (kbd eaf-evil-leader-key))
;;             ("image-viewer" (kbd eaf-evil-leader-key))
;;             (_  (kbd "SPC")))
;;         (kbd "SPC")))))

(with-eval-after-load 'treemacs
  (progn
    (require 'treemacs-nerd-icons)
    (treemacs-load-theme "nerd-icons")))

;; From https://kitchingroup.cheme.cmu.edu/blog/2017/06/10/Adding-keymaps-to-src-blocks-via-org-font-lock-hook/
(require 'lispy)
(require 'elpy)

(setq scimax-src-block-keymaps
      `(("ipython" . ,(let ((map (make-composed-keymap
                                  `(,elpy-mode-map ,python-mode-map ,pyvenv-mode-map)
                                  org-mode-map)))
                        ;; In org-mode I define RET so we f
                        (define-key map (kbd "<return>") 'newline)
                        (define-key map (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)
                        map))
        ("python" . ,(let ((map (make-composed-keymap
                                 `(,elpy-mode-map ,python-mode-map ,pyvenv-mode-map)
                                 org-mode-map)))
                       ;; In org-mode I define RET so we f
                       (define-key map (kbd "<return>") 'newline)
                       (define-key map (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)
                       map))
        ("emacs-lisp" . ,(let ((map (make-composed-keymap `(,lispy-mode-map
                                                            ,emacs-lisp-mode-map
                                                            ,outline-minor-mode-map)
                                                          org-mode-map)))
                           (define-key map (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)
                           map))))

(defun scimax-add-keymap-to-src-blocks (limit)
  "Add keymaps to src-blocks defined in `scimax-src-block-keymaps'."
  (let ((case-fold-search t)
        lang)
    (while (re-search-forward org-babel-src-block-regexp limit t)
      (let ((lang (match-string 2))
            (beg (match-beginning 0))
            (end (match-end 0)))
        (if (assoc (org-no-properties lang) scimax-src-block-keymaps)
            (progn
              (add-text-properties
               beg end `(local-map ,(cdr (assoc
                                          (org-no-properties lang)
                                          scimax-src-block-keymaps))))
              (add-text-properties
               beg end `(cursor-sensor-functions
                         ((lambda (win prev-pos sym)
                            ;; This simulates a mouse click and makes a menu change
                            (org-mouse-down-mouse nil)))))))))))

(defun scimax-spoof-mode (orig-func &rest args)
  "Advice function to spoof commands in org-mode src blocks.
It is for commands that depend on the major mode. One example is
`lispy--eval'."
  (if (org-in-src-block-p)
      (let ((major-mode (intern (format "%s-mode" (first (org-babel-get-src-block-info))))))
        (apply orig-func args))
    (apply orig-func args)))

(define-minor-mode scimax-src-keymap-mode
  "Minor mode to add mode keymaps to src-blocks."
  :init-value nil
  (if scimax-src-keymap-mode
      (progn
        (add-hook 'org-font-lock-hook #'scimax-add-keymap-to-src-blocks t)
        (add-to-list 'font-lock-extra-managed-props 'local-map)
        (add-to-list 'font-lock-extra-managed-props 'cursor-sensor-functions)
        (advice-add 'lispy--eval :around 'scimax-spoof-mode)
        (cursor-sensor-mode +1))
    (remove-hook 'org-font-lock-hook #'scimax-add-keymap-to-src-blocks)
    (advice-remove 'lispy--eval 'scimax-spoof-mode)
    (cursor-sensor-mode -1))
  (font-lock-fontify-buffer))

(add-hook 'org-mode-hook (lambda ()
                           (scimax-src-keymap-mode +1)))

(use-package! ellama
        :ensure t
        :bind ("C-c e" . ellama)
        ;; setup key bindings
        ;; (setopt ellama-keymap-prefix "C-c e")
        ;; language you want ellama to translate to
        :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
        :init
        (setopt ellama-auto-scroll t)
        (setopt ellama-language "Esperanto")
        ;; could be llm-openai for example
        (require 'llm-ollama)
        (setopt ellama-provider
                (make-llm-ollama
                ;; this model should be pulled to use it
                ;; value should be the same as you print in terminal during pull
                :chat-model "llama3:8b-instruct-q8_0"
                :embedding-model "nomic-embed-text"
                :default-chat-non-standard-params '(("num_ctx" . 8192))))
        (setopt ellama-summarization-provider
                (make-llm-ollama
                :chat-model "qwen2.5:3b"
                :embedding-model "nomic-embed-text"
                :default-chat-non-standard-params '(("num_ctx" . 32768))))
        (setopt ellama-coding-provider
                (make-llm-ollama
                :chat-model "deepseek-coder-v2"
                :embedding-model "nomic-embed-text"
                :default-chat-non-standard-params '(("num_ctx" . 32768))))
        ;; Naming new sessions with llm
        (setopt ellama-naming-provider
                (make-llm-ollama
                :chat-model "llama3:8b-instruct-q8_0"
                :embedding-model "nomic-embed-text"
                :default-chat-non-standard-params '(("stop" . ("\n")))))
        (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
        ;; Translation llm provider
        (setopt ellama-translation-provider
                (make-llm-ollama
                :chat-model "qwen2.5:3b"
                :embedding-model "nomic-embed-text"
                :default-chat-non-standard-params
                '(("num_ctx" . 32768))))
        ;; customize display buffer behaviour
        ;; see ~(info "(elisp) Buffer Display Action Functions")~
        :config
        (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
        (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
        (advice-add 'pixel-scroll-precision :before #'ellama-disable-scroll)
        (advice-add 'end-of-buffer :after #'ellama-enable-scroll)
)
