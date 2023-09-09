;;; llama-cpp-chat.el --- A client for llama-cpp server -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Evgeny Kurnevsky <kurnevsky@gmail.com>

;; Version: 1.0.0
;; Author: Evgeny Kurnevsky <kurnevsky@gmail.com>
;; Keywords: llama, llm, ai
;; URL: https://github.com/kurnevsky/llama.el

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

(require 'llama)

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

(defcustom llama-cpp-chat-prompt "### System Prompt
You are an intelligent programming assistant."
  "Llama prompt to start chat with."
  :type 'string
  :group 'llama)

(defcustom llama-cpp-chat-input-prefix "

### User Message
"
  "String to prefix user inputs with."
  :type 'string
  :group 'llama)

(defcustom llama-cpp-chat-input-suffix "
### Assistant
"
  "String to suffix after user inputs with."
  :type 'string
  :group 'llama)

(defconst llama-cpp-chat--buffer-name "*llama*")

(defun llama-cpp-chat-insert-prompt ()
  "Insert chat prompt at point."
  (insert (propertize llama-cpp-chat-prompt 'face 'llama-cpp-chat-prompt-face)))

(defun llama-cpp-chat-insert-input-prefix ()
  "Insert the input prefix at point."
  (insert (propertize llama-cpp-chat-input-prefix 'face 'llama-cpp-chat-input-prefix-face))
  (unless (string-empty-p llama-cpp-chat-input-prefix)
    (set-text-properties (1- (point)) (point) nil)))

(defun llama-cpp-chat-insert-input-suffix ()
  "Insert the input suffix at point."
  (insert (propertize llama-cpp-chat-input-suffix 'face 'llama-cpp-chat-input-suffix-face))
  (unless (string-empty-p llama-cpp-chat-input-suffix)
    (set-text-properties (1- (point)) (point) nil)))

;;;###autoload
(defun llama-cpp-chat-start ()
  "Start a Llama chat session.

This interactive function creates and displays a new buffer for a Llama chat
session.  A prompt message for the Llama chat and an input prefix are inserted
into the buffer."
  (interactive)
  (with-current-buffer (get-buffer-create llama-cpp-chat--buffer-name)
    (erase-buffer)
    (llama-cpp-chat-insert-prompt)
    (llama-cpp-chat-insert-input-prefix))
  (display-buffer llama-cpp-chat--buffer-name))

(defun llama-cpp-chat-complete ()
  "Complete text from llama buffer."
  (interactive)
  (with-current-buffer (get-buffer-create llama-cpp-chat--buffer-name)
    (llama-cpp-complete (buffer-string) (lambda (token stop)
                                      (with-current-buffer (get-buffer-create llama-cpp-chat--buffer-name)
                                        (save-excursion
                                          (goto-char (point-max))
                                          (insert token)
                                          (when stop
                                            (llama-cpp-chat-insert-input-prefix))))))))

(provide 'llama-cpp-chat)
;;; llama-cpp-chat.el ends here

;; Local Variables:
;; End:

