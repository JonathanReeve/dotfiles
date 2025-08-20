;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! pdf-tools :built-in 'prefer)

;; Read epubs
(package! nov)

;; See https://github.com/fuxialexander/org-pdftools
;; (package! org-pdftools)

;; Use BibSonomy
;; (package! biblio-bibsonomy)

;; Change themes with PyWal
(package! ewal-doom-themes)

;; Query Org files
;; (package! org-ql)

;; Make eshell better
;; (package! fish-completion)

;; Edit Turtle files
;; (package! ttl-mode)

(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

(package! org-ref)

;; (package! org-cite-csl-activate :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate" :files ("*.el")))

(package! org-clock-reminder
  :recipe (:host github :repo "inickey/org-clock-reminder"))

;; ~/.doom.d/packages.el
(package! evil-colemak-basics) ; colemak remaps

;; (package! radar
;;   :recipe (:type git
;;            :repo "git@github.pie.apple.com:emacs/emacs-radar.git"))

;; Workaround to an org/org-roam bug;
;; see https://github.com/org-roam/org-roam/issues/2361
;; (package! org :pin "ca873f7")

(package! ob-mermaid)

(package! nushell-mode)

;; Workaround for icon issue
;;See https://github.com/doomemacs/doomemacs/issues/7519
;; (package! treemacs :pin "7c6628a241845cd8f626d2603f747f777de4eefb")
;; (package! lsp-treemacs :disable)

(package! jsonnet-mode)

(package! ob-nushell
  :recipe (:type git
           :repo "https://github.com/ln-nl/ob-nushell.git")
  :pin "767c8cfdaf929d9347f64ea44c6846b1c5e86da5")

;; Needed by scimax- hacks for org-babel
(package! lispy)

(package! org-node)

(package! ellama :pin "8281a9847b1a35df5433d93a8e7569bbe7ef1215")

(package! turtle-mode
  :recipe (:type git
           :repo "https://github.com/drobilla/turtle-mode")
  :pin "8eb2ec3fee0a28f3716371c930b04be9cd639811"
  )

(package! aider :recipe (:host github :repo "tninja/aider.el" ))

(package! apple-ldap
    :recipe (:type git :host nil :repo "git@github.pie.apple.com:emacs/apple-ldap.git")
    :pin "968e5f878127e0968bd8041bc7e5800128580814")

(package! treemacs-nerd-icons)

(package! claude-code
  :recipe (:host github
           :repo "stevemolitor/claude-code.el"
           :branch "main"
           :files ("*.el" (:exclude "images/*"))))

(package! org-noter
  :recipe (:host github
           :repo "ahmed-shariff/org-noter"
           ))
