;;; private/gptel/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :config
  (setq gptel-api-key (lambda () (auth-source-pick-first-password
                                  :host "api.google.com"
                                  :user "apikey")))
  (setq gptel-backend (gptel-make-gemini "Gemini"
                        :key gptel-api-key
                        :stream t))
  (setq gptel-model 'gemini-2.0-flash))

(use-package! gptel-agent
  :after gptel
  :config
  (gptel-agent-mode 1))
