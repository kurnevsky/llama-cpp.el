;;; llama-cpp-code.el --- A client for llama-cpp server -*- lexical-binding: t; -*-

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

(require 'llama-cpp)
(require 'llama-cpp-chat)

(defcustom llama-cpp-code-lang-modes
  '((tuareg-mode . "ocaml")
    (emacs-lisp-mode . "elisp")
    (artist-mode . "ditaa")
    (asy-mode . "asymptote")
    (shell-script-mode . "screen")
    (sh-mode . "shell"))
  "Alist mapping major modes to their language."
  :type '(repeat
          (cons
           (symbol "Major mode")
           (string "Language name")))
  :group 'llama)

(defcustom llama-cpp-code-region-prompt "%s
```%s
%s
```
"
  "Llama code task prompt."
  :type 'string)

(defun llama-cpp-code-lang-to-mode (mode)
  "Return text language for a major MODE."
  (or
   (alist-get mode llama-cpp-code-lang-modes)
   (string-remove-suffix "-mode" (string-remove-suffix "-ts-mode" (symbol-name mode)))))

;;;###autoload
(defun llama-cpp-code-region-task (start end question)
  "Ask the llama to perform a task within the specified region.
The task is defined by the text in the current buffer between START and END.
The QUESTION argument is a string asking for clarification or more information
about the task."
  (interactive "r\nsDescribe your task: ")
  (llama-cpp-chat-start)
  (let ((prompt (format llama-cpp-code-region-prompt
                        question
                        (llama-cpp-code-lang-to-mode major-mode)
                        (buffer-substring-no-properties start end))))
    (with-current-buffer (get-buffer-create llama-cpp-chat--buffer-name)
      (insert prompt)
      (llama-cpp-chat-insert-input-suffix)))
  (llama-cpp-chat-complete))

(provide 'llama-cpp-code)
;;; llama-cpp-code.el ends here

;; Local Variables:
;; End:
