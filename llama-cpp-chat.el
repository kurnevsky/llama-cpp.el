;;; llama-cpp-chat.el --- A client for llama-cpp server -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Evgeny Kurnevsky <kurnevsky@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; llama-cpp client

;;; Code:

(require 'llama-cpp)
(require 'llama-cpp-template)

(defface llama-cpp-chat-prompt-face
  '((t :inherit font-lock-comment-face))
  "Face used for the prompt."
  :group 'llama)

(defface llama-cpp-chat-input-prefix-face
  '((t :inherit font-lock-string-face))
  "Face used for the input prefix."
  :group 'llama)

(defface llama-cpp-chat-input-suffix-face
  '((t :inherit font-lock-variable-name-face))
  "Face used for the input suffix."
  :group 'llama)

(defcustom llama-cpp-chat-input-prefix-display "\n> "
  "Display property for the input prefix."
  :type '(choice
          (const :tag "Display as is" nil)
          (string :tag "Display as string"))
  :group 'llama)

(defcustom llama-cpp-chat-input-suffix-display "\n>> "
  "Display property for the input suffix."
  :type '(choice
          (const :tag "Display as is" nil)
          (string :tag "Display as string"))
  :group 'llama)

(defcustom llama-cpp-chat-prompt-prefix-display ""
  "Display property for the prompt prefix."
  :type 'string
  :group 'llama)

(defcustom llama-cpp-chat-prompt "You are a helpful AI assistant."
  "Llama prompt to start chat with."
  :type 'string
  :group 'llama)

(defcustom llama-cpp-highlight-probabilities nil
  "Whether to highlight probabilities of tokens with color."
  :type 'boolean
  :group 'llama)

(defvaralias 'llama-cpp-chat-prompt-prefix 'llama-cpp-template-prompt-prefix)
(defvaralias 'llama-cpp-chat-input-prefix 'llama-cpp-template-input-prefix)
(defvaralias 'llama-cpp-chat-input-suffix 'llama-cpp-template-input-suffix)

(defconst llama-cpp-chat--buffer-name "*llama*")

(defun llama-cpp-chat-insert-prompt ()
  "Insert chat prompt at point."
  (when llama-cpp-chat-prompt-prefix
    (insert (propertize llama-cpp-chat-prompt-prefix
                        'face 'llama-cpp-chat-prompt-face
                        'display llama-cpp-chat-prompt-prefix-display
                        'read-only t
                        'rear-nonsticky t))
    (insert (propertize llama-cpp-chat-prompt
                        'face 'llama-cpp-chat-prompt-face
                        'front-sticky t))))

(defun llama-cpp-chat-insert-input-prefix ()
  "Insert the input prefix at point."
  (insert (propertize llama-cpp-chat-input-prefix
                      'face 'llama-cpp-chat-input-prefix-face
                      'display llama-cpp-chat-input-prefix-display
                      'read-only t
                      'rear-nonsticky t)))

(defun llama-cpp-chat-insert-input-suffix ()
  "Insert the input suffix at point."
  (insert (propertize llama-cpp-chat-input-suffix
                      'face 'llama-cpp-chat-input-suffix-face
                      'display llama-cpp-chat-input-suffix-display
                      'read-only t
                      'rear-nonsticky t)))

;;;###autoload
(defun llama-cpp-chat-start ()
  "Start a Llama chat session.

This interactive function creates and displays a new buffer for a Llama chat
session.  A prompt message for the Llama chat and an input prefix are inserted
into the buffer."
  (interactive)
  (with-current-buffer (get-buffer-create llama-cpp-chat--buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (llama-cpp-chat-insert-prompt)
      (llama-cpp-chat-insert-input-prefix)))
  (pop-to-buffer llama-cpp-chat--buffer-name)
  (goto-char (point-max))
  (llama-cpp-chat-mode t))

(defun llama-cpp-probability-color (probability)
  "Return a color for the token PROBABILITY."
  (let ((c (* probability 255)))
    (format "#%02X%02X%02X" c c 255)))

(defun llama-cpp-chat-complete ()
  "Complete text from the llama buffer."
  (interactive)
  (with-current-buffer (get-buffer-create llama-cpp-chat--buffer-name)
    (llama-cpp-complete (buffer-string) (lambda (json)
                                          (let* ((content (plist-get json :content))
                                                 (stop (eq (plist-get json :stop) t))
                                                 (completion-probabilities (plist-get json :completion_probabilities))
                                                 (probs (-some (lambda (item) (when (string= (plist-get item :content) content)
                                                                                (plist-get item :probs)))
                                                               (append completion-probabilities nil)))
                                                 (probability (-some (lambda (item)
                                                                       (when (string= (plist-get item :tok_str) content)
                                                                         (plist-get item :prob)))
                                                                     (append probs nil))))
                                            (with-current-buffer (get-buffer-create llama-cpp-chat--buffer-name)
                                              (save-excursion
                                                (goto-char (point-max))
                                                (insert (if llama-cpp-highlight-probabilities
                                                            (propertize content
                                                                        'face `(foreground-color . ,(llama-cpp-probability-color (or probability 0)))
                                                                        'rear-nonsticky t)
                                                          content)))
                                              (when stop
                                                (goto-char (point-max))
                                                (llama-cpp-chat-insert-input-prefix))))))))

(defun llama-cpp-chat-answer ()
  "Continue the chat session in the llama buffer.

This function is intended for use during an active chat session with the
llama.  It appends a predefined input suffix to the end of the current buffer
and then proceeds to complete the chat session."
  (interactive)
  (with-current-buffer (get-buffer-create llama-cpp-chat--buffer-name)
    (save-excursion
      (goto-char (point-max))
      (llama-cpp-chat-insert-input-suffix)))
  (llama-cpp-chat-complete))

(define-minor-mode llama-cpp-chat-mode
  "Toggle llama-cpp chat mode."
  :lighter "llama-cpp"
  :keymap `((,(kbd "<return>") . llama-cpp-chat-answer)
            (,(kbd "S-<return>") . newline)))

(provide 'llama-cpp-chat)
;;; llama-cpp-chat.el ends here

;; Local Variables:
;; End:
