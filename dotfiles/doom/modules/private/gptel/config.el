;;; private/gptel/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :config
  (setq gptel-api-key 
        (lambda () 
          (require 'auth-source-pass)
          ;; Ensure we look in the correct password-store directory
          (let ((auth-source-pass-filename "/home/jon/Dokumentoj/Personal/.password-store"))
            (let ((key (auth-source-pass-get 'secret "openrouter.ai/apikey")))
              (unless key
                (error "GPTel: API key not found in pass (entry: openrouter.ai/apikey)"))
              key))))

  ;; (setq gptel-backend (gptel-make-gemini "Gemini"
  ;;                       :key gptel-api-key
  ;;                       :stream t))

  (setq gptel-backend (gptel-make-openai "OpenRouter"
                        :host "openrouter.ai"
                        :endpoint "/api/v1/chat/completions"
                        :key gptel-api-key
                        :stream t
                        :models '(google/gemini-3-flash-preview
                                  openai/gpt-3.5-turbo
                                  )))
  (setq gptel-model 'google/gemini-3-flash-preview)

(defun my/gptel-agent--execute-nushell (callback command)
  "Execute COMMAND asynchronously in nushell and call CALLBACK with output."
  (let* ((output-buffer (generate-new-buffer " *gptel-agent-nushell*"))
         (proc (make-process
                :name "gptel-agent-nushell"
                :buffer output-buffer
                :command (list "nu" "-c" command)
                :connection-type 'pipe
                :sentinel
                (lambda (process _event)
                  (when (memq (process-status process) '(exit signal))
                    (let* ((exit-code (process-exit-status process))
                           (output (with-current-buffer (process-buffer process)
                                     (buffer-string))))
                      (kill-buffer (process-buffer process))
                      (funcall callback
                               (if (zerop exit-code)
                                   output
                                 (format "Error (exit code %d):\n%s" exit-code output)))))))))
    (set-process-query-on-exit-flag proc nil)))

(use-package! gptel-agent
  :after gptel
  :config
  (setq gptel-agent-dirs '("~/Agordoj/agents/"))
  (gptel-agent-mode 1)

  ;; Register Nushell tool
  (gptel-make-tool
   :name "Nushell"
   :function #'my/gptel-agent--execute-nushell
   :description "Execute Nushell commands. 
This tool provides access to a Nushell environment.
- Quote file paths with spaces using double quotes.
- Chain dependent commands with pipelines or ;
- Use absolute paths instead of cd when possible"
   :args '((:name "command"
            :type string
            :description "The Nushell command to execute"))
   :async t
   :include t)

  (gptel-agent-update))

(use-package! mcp
  :after gptel
  :config
  (require 'mcp-hub)
  (require 'gptel-integrations)
  (setq mcp-hub-servers
        '(("playwright" . (:command "playwright-mcp" :args ()))))
  (add-hook 'gptel-mode-hook #'gptel-mcp-connect))
