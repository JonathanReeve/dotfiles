;; -*- no-byte-compile: t; -*-
;;; private/vulpea/packages.el

(package! vulpea)
(package! citar-vulpea)
(package! vulpea-ui :recipe (:host github :repo "d12frosted/vulpea" :files ("vulpea-ui.el")))
(package! vulpea-journal :recipe (:host github :repo "d12frosted/vulpea" :files ("vulpea-journal.el")))
(package! consult-vulpea :recipe (:host github :repo "d12frosted/vulpea" :files ("consult-vulpea.el")))
