;;; llama-cpp-template.el --- A client for llama-cpp server -*- lexical-binding: t; -*-

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

(require 'dash)

(defconst llama-cpp-template--templates
  '(
    (chatml . ("<|im_start|>system\n" "<|im_end|>\n<|im_start|>user\n" "<|im_end|>\n<|im_start|>assistant\n"))
    (mistral . (nil " [INST] " " [/INST] "))
    (llama3 . ("<|start_header_id|>system<|end_header_id|>\n\n" "<|eot_id|><|start_header_id|>user<|end_header_id|>\n\n" "<|eot_id|><|start_header_id|>assistant<|end_header_id|>\n\n"))
    ))

(defcustom llama-cpp-template-prompt-prefix "<|im_start|>system\n"
  "Llama prompt prefix."
  :type 'string
  :group 'llama)

(defcustom llama-cpp-template-input-prefix "<|im_end|>\n<|im_start|>user\n"
  "String to prefix user inputs with."
  :type 'string
  :group 'llama)

(defcustom llama-cpp-template-input-suffix "<|im_end|>\n<|im_start|>assistant\n"
  "String to suffix after user inputs with."
  :type 'string
  :group 'llama)

(defun llama-cpp-template-get (template)
  "Get prompt prefix, input prefix and input suffix for TEMPLATE."
  (alist-get template llama-cpp-template--templates))

(defun llama-cpp-template-set (template)
  "Set prompt prefix, input prefix and input suffix for TEMPLATE."
  (-let [(prompt-prefix input-prefix input-suffix) (llama-cpp-template-get template)]
    (setq llama-cpp-template-prompt-prefix prompt-prefix
          llama-cpp-template-input-prefix input-prefix
          llama-cpp-template-input-suffix input-suffix)))

(defun llama-cpp-template-select ()
  "Select llama-cpp template."
  (interactive)
  (when-let ((options '(
                        ("ChatML" . chatml)
                        ("Llama 3" . llama3)
                        ("Mistral" . mistral)
                        ))
             (template (completing-read
                        "Which template would you like to use?"
                        options
                        nil
                        t)))
    (llama-cpp-template-set (alist-get template options nil nil #'string=))))

(provide 'llama-cpp-template)
;;; llama-cpp-template.el ends here

;; Local Variables:
;; End:
