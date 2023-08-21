; -----------------------------------------------------------------------------
; ChatGPT interface. Use M-x package-install to install `request` package.
(require 'request)

(setq max-specpdl-size 65536)   ;; Default is "only" 1600.
(setq max-lisp-eval-depth 4096) ;; Default is 500. I should look into why I need this.

(setq openai-api-key-file "~/.openai-api-key.txt")
(setq openai-api-chat-url "https://api.openai.com/v1/chat/completions")
(setq openai-chatgpt-model "gpt-3.5-turbo")
(setq chatbot-buffer-alist '())
(setq chatbot-user-name "#==> User")
(setq chatbot-bot-name  "#--> ChatGPT")

;; 0 means not to set it. The default appears to be 'inf' at the API
;; reference site, but the error message says 4097 for gpt-3.5-turbo; the
;; limit includes the message tokens.
(setq openai-max-tokens 0)
(setq openai-request-timeout 180)

(defun read-api-key (filename)
  "Read OpenAI API key from a file."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

;; The request package fails to handle responses if this isn't defined:
(if (not (boundp 'auto-revert-notify-watch-descriptor-hash-list))
    (setq auto-revert-notify-watch-descriptor-hash-list '()))

(defun openai-gpt-chat-request (prompt)
  "Send a request to the OpenAI API with given PROMPT. Return the response as a string."
  (let* ((api-key (read-api-key openai-api-key-file))
         (url openai-api-chat-url)
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " (string-trim api-key)))))
         (messages `((("role" . "system") ("content" . "Helpful and terse assistant."))
                     (("role" . "user") ("content" . ,prompt))))
         (base-alist `(("messages" . ,messages)
                       ("model" . ,openai-chatgpt-model)))
         (full-alist (if (> openai-max-tokens 0)
                         (append base-list `("max_tokens" . ,openai-max-tokens))
                       base-alist))
         (data (json-encode full-alist))
         (result nil))
    ;; (message (format "Data = '%s'\n" data))
    (message (format "Check buffer '*chat*'. Request could taken %s..."
                     (if (= openai-request-timeout 0)
                         "a while"
                       (format "up to %d seconds" openai-request-timeout))))
    (request
     url
     :type "POST"
     :timeout openai-request-timeout
     :headers headers
     :data data
     :parser 'json-read
     :sync t
     :complete (cl-function
                (lambda (&key data &allow-other-keys)
                  ;; (message (format "Result in cl-function: %s" data))
                  (setq result (format "%s\n"
                                  (assoc-default 'content
                                    (assoc-default 'message
                                      (elt (assoc-default 'choices data) 0))))))))
    result))

(defun chatgpt-single-prompt-in-chat-buffer (prompt)
  "Send a request to the OpenAI API with a given PROMPT and insert the response into a buffer."
  (interactive "sEnter the prompt: ")
  (let ((response (openai-gpt-chat-request prompt)))
    (with-current-buffer (get-buffer-create "*chat*")
      (goto-char (point-max))
      (insert (format "\n\n%s\n\n" chatbot-user-name))
      (insert prompt)
      (insert (format "\n\n%s\n\n" chatbot-bot-name))
      (insert response))))

; -----------------------------------------------------------------------------

(defun remove-first-if-empty (lines)
  "If the first line of `lines` is empty, remove it."
  (if (null lines) '()
    (if (equal (car lines) "") (cdr lines)
      lines)))

(defun buffer-lines-list (buffer-name)
  (let ((buffer (get-buffer-create buffer-name)))
    (save-current-buffer
      (set-buffer buffer)
      (split-string (buffer-string) "\n"))))

(setq chat-buffer-role-lines `((user . ,chatbot-user-name) (bot . ,chatbot-bot-name)))
(setq chatgpt-role-names '((user . "user") (bot . "assistant")))

(defun chatbot-other-role (role)
  (if (eq role 'user) 'bot 'user))

(defun add-new-line (message line)
  (if (equal message "") line
    (concat message "\n" line)))

(defun chatbot-new (current-role current-message reversed-messages)
  (list current-role current-message reversed-messages))
(defun chatbot-init () (chatbot-new 'user "" '()))
(defun chatbot-role (obj) (car obj))
(defun chatbot-message (obj) (cadr obj))
(defun chatbot-messages (obj) (caddr obj))  ;; messages are in reverse order

(defun chatbot-set-role (obj role)
  (chatbot-new role (chatbot-message obj) (chatbot-messages obj)))

(defun chatbot-role-line (obj)
  (cdr (assoc (chatbot-role obj) chatbot-buffer-role-lines)))
(defun chatbot-next-role (obj)
  (if (eq (chatbot-role obj) 'user) 'bot 'user))
(defun chatbot-next-role-line (obj)
  (cdr (assoc (chatbot-next-role obj) chatbot-buffer-role-lines)))

(defun chatbot-add-message-line (obj line)
  (let ((message (chatbot-message obj)))
    (chatbot-new (chatbot-role obj)
                 (if (equal "" message) line (concat message "\n" line))
                 (chatbot-messages obj))))

(defun chatgpt-message (role content)
  `(("role" . ,(cdr (assoc role chatgpt-role-names)))
    ("content" . ,content)))

(defun chatbot-chatgpt-message (obj)
  (chatgpt-message (chatbot-role obj) (chatbot-message obj)))

(defun chatbot-absorb-chatgpt-message (obj)
  (if (equal "" (chatbot-message obj))
      obj
    (chatbot-new (chatbot-role obj)
                 ""
                 (cons (chatbot-chatgpt-message obj)
                       (chatbot-messages obj)))))
      
(defun chatbot-finalize (obj)
  (chatbot-set-role (chatbot-absorb-chatgpt-message obj) 'user))

(defun chatbot-next-chatgpt (obj)
  (chatbot-new (chatbot-next-role obj) ""
               (cons (chatgpt-message (chatbot-role obj) (chatbot-message obj))
                     (chatbot-messages obj))))

(defun chatbot-final-chatgpt-message-array (obj)
  (if (equal "" (chatbot-message obj))
      (nreverse (chatbot-messages obj))
    (chatbot-final-chatgpt-message-array (chatbot-next-chatgpt obj))))

(defun chatbot-parse-lines (obj lines)
  (if (null lines)
      (chatbot-finalize obj)
      ; (chatbot-final-chatgpt-message-array obj)
    (let* ((line (car lines))
           (role (car obj))
           (message (cadr obj))
           (rev-messages (caddr obj))
           (role-buffer-line (cdr (assoc role chat-buffer-role-lines)))
           (next-role (chatbot-other-role role))
           (next-role-buffer-line (cdr (assoc next-role chat-buffer-role-lines))))
      (if (member line (list "" role-buffer-line))
          (chatbot-parse-lines obj (cdr lines))
        (if (equal line next-role-buffer-line)
            (chatbot-parse-lines (chatbot-next-chatgpt obj) (cdr lines))
          (chatbot-parse-lines (chatbot-add-message-line obj line) (cdr lines)))))))

(defun chatbot-parse-buffer (buffer-name)
  (chatbot-finalize (chatbot-parse-lines (chatbot-init) (buffer-lines-list buffer-name))))

(defun openai-gpt-chat-full-request (messages)
  "Send a request to the OpenAI API with given MESSAGES. Return the response as a string."
  (let* ((api-key (read-api-key openai-api-key-file))
         (url openai-api-chat-url)
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " (string-trim api-key)))))
         (base-alist `(("messages" . ,messages)
                       ("model" . ,openai-chatgpt-model)))
         (full-alist (if (> openai-max-tokens 0)
                         (append base-list `("max_tokens" . ,openai-max-tokens))
                       base-alist))
         (data (json-encode full-alist))
         (result nil))
    ;; (message (format "Data = '%s'\n" data))
    (message (format "Check buffer '*chat*'. Request could taken %s..."
                     (if (= openai-request-timeout 0)
                         "a while"
                       (format "up to %d seconds" openai-request-timeout))))
    (request
     url
     :type "POST"
     :timeout openai-request-timeout
     :headers headers
     :data data
     :parser 'json-read
     :sync t
     :complete (cl-function
                (lambda (&key data &allow-other-keys)
                  ;; (message (format "Result in cl-function: %s" data))
                  (setq result (format "%s\n"
                                  (assoc-default 'content
                                    (assoc-default 'message
                                      (elt (assoc-default 'choices data) 0))))))))
    result))

(defun chatgpt-extend-chat-buffer (prompt)
  "Send a request to the OpenAI API with a given PROMPT and insert the response into a buffer."
  (interactive "sEnter the prompt: ")
  (let*
      ((chatbot (chatbot-add-message-line (chatbot-parse-buffer "*chat*") prompt))
       (messages (chatbot-final-chatgpt-message-array chatbot))
       (response (openai-gpt-chat-full-request messages)))
    (with-current-buffer (get-buffer-create "*chat*")
      (goto-char (point-max))
      (insert (format "\n\n%s\n\n" chatbot-user-name))
      (insert prompt)
      (insert (format "\n\n%s\n\n" chatbot-bot-name))
      (insert response))))


;(global-set-key (kbd "C-c C") 'openai-gpt-chat-buffer)
(global-set-key "\C-cCp" 'chatgpt-single-prompt-in-chat-buffer)
(global-set-key "\C-cCc" 'chatgpt-extend-chat-buffer)

; (openai-gpt-chat "Translate the following English text to French: '{'Hello, world!'}'")
