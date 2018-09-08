#!/run/current-system/sw/bin/python

colors = open('/home/jon/.cache/wal/colors').read().split('\n')[:-1]

template = """
(require 'base16-theme)

;; colours generated dynamically by wal
(defun set-wal-colors () (setq base16-wal-colors
			       '(:base00 "{}"
					 :base01 "{}"
					 :base02 "{}"
					 :base03 "{}"
					 :base04 "{}"
					 :base05 "{}"
					 :base06 "{}"
					 :base07 "{}"
					 :base08 "{}"
					 :base09 "{}"
					 :base0A "{}"
					 :base0B "{}"
					 :base0C "{}"
					 :base0D "{}"
					 :base0E "{}"
					 :base0F "{}")))

(defvar base16-wal-colors nil "All colors for base16-wal are defined here.")
(set-wal-colors)

;; Define the theme
(deftheme base16-wal)

;; Add all the faces to the theme
(base16-theme-define 'base16-wal base16-wal-colors)

;; Mark the theme as provided
(provide-theme 'base16-wal)

(provide 'base16-wal)
""".format(*colors)

open('/home/jon/.emacs.d/private/base16-wal-theme.el', 'w').write(template)
