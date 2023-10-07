;;; llama-cpp.el --- A client for llama-cpp server -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Evgeny Kurnevsky <kurnevsky@gmail.com>

;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (dash "2.19.1"))
;; Author: Evgeny Kurnevsky <kurnevsky@gmail.com>
;; Keywords: tools
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

;; An Emacs client for interacting with the `llama-cpp' server:
;; https://github.com/ggerganov/llama.cpp/tree/master/examples/server

;;; Code:

(require 'url-http)
(require 'dash)

(defgroup llama-cpp nil
  "Llama-cpp client."
  :group 'tools)

(defcustom llama-cpp-host "localhost"
  "Host of the llama-cpp server."
  :type 'string
  :group 'llama)

(defcustom llama-cpp-port 8080
  "Port of the llama-cpp server."
  :type 'natnum
  :group 'llama)

(defcustom llama-cpp-params '(:n_predict -1 :mirostat 2)
  "Parameters for the llama /completion request."
  :type '(alist :key-type (symbol :tag "Parameter")
                :value-type (sexp :tag "Value"))
  :group 'llama)

(defconst llama-cpp--process "llama")
(defconst llama-cpp--process-buffer " *llama-cpp-output*")
(defconst llama-cpp--rx (rx bol "data: " (group (+ nonl)) eol "\n"))

(defvar-local llama-cpp--start 0)

(defun llama-cpp-cancel ()
  "Cancel the running llama process.
It will terminate TCP connection and stop server computations."
  (interactive)
  (when-let ((process (get-process llama-cpp--process)))
    (delete-process process))
  (when-let ((buffer (get-buffer llama-cpp--process-buffer)))
    (kill-buffer buffer)))

(defun llama-cpp--completion-url ()
  "Llama-cpp completion URL."
  (format "http://%s:%d/completion" llama-cpp-host llama-cpp-port))

(defun llama-cpp--request-body (prompt)
  "Llama-cpp POST request body for the PROMPT."
  (defvar url-http-method)
  (defvar url-http-proxy)
  (defvar url-http-target-url)
  (defvar url-http-referer)
  (defvar url-http-extra-headers)
  (defvar url-http-data)
  (let ((url-http-method "POST")
        (url-http-proxy nil)
        (url-http-target-url (url-generic-parse-url (llama-cpp--completion-url)))
        (url-http-referer nil)
        (url-http-extra-headers `(("Content-Type" . "application/json; charset=utf-8")))
        (url-http-data
         (encode-coding-string
          (json-serialize (append `(:prompt ,prompt :stream t) llama-cpp-params)) 'utf-8 t)))
    (url-http-create-request)))

(defun llama-cpp--process-filter (callback proc data)
  "Llama process filter function.
CALLBACK is called on each chunked response.
PROC and DATA are the filter params."
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (let ((mark (process-mark proc)))
            (goto-char mark)
            (insert data)
            (set-marker mark (point))
            (let ((s (buffer-string)))
              (save-match-data
                (while (string-match llama-cpp--rx s llama-cpp--start)
                  (setq llama-cpp--start (1+ (match-end 0)))
                  (let ((json (json-parse-string (substring s (match-beginning 1) (match-end 1)) :object-type 'plist)))
                    (funcall callback
                             (plist-get json :content)
                             (eq (plist-get json :stop) t))))))))))))

;;;###autoload
(defun llama-cpp-complete (prompt callback)
  "Complete the PROMPT using llama-cpp server.
CALLBACK is called multiple times after a new token generated.

It cancels the previous running llama generation if any."
  (llama-cpp-cancel)
  (let* ((buffer (get-buffer-create llama-cpp--process-buffer))
         (process (make-network-process
                   :name llama-cpp--process
                   :buffer buffer
                   :host llama-cpp-host
                   :service llama-cpp-port
                   :filter (-partial #'llama-cpp--process-filter callback))))
    (process-send-string process (llama-cpp--request-body prompt))))

(provide 'llama-cpp)
;;; llama-cpp.el ends here

;; Local Variables:
;; End:
