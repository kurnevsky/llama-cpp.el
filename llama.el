;;; llama.el --- A client for llama-cpp server -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Evgeny Kurnevsky <kurnevsky@gmail.com>

;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (dash "2.19"))
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

(require 'url-http)
(require 'dash)

(defgroup llama nil
  "Llama-cpp client."
  :group 'tools)

(defcustom llama-host "pc"
  "Host of the llama-cpp server."
  :type 'string
  :group 'llama)

(defconst llama--process "llama")
(defconst llama--process-buffer " *llama-output*")
(defconst llama--rx (rx bol "data: " (group (+ nonl)) eol))

(defvar-local llama--start 0)

(defcustom llama-port 8080
  "Port of the llama-cpp server."
  :type 'natnum
  :group 'llama)

(defun llama-cancel ()
  "Cancel the running llama process.
It will terminate TCP connection and stop server computations."
  (interactive)
  (when-let ((process (get-process llama--process)))
    (delete-process process))
  (when-let ((buffer (get-buffer llama--process-buffer)))
    (kill-buffer buffer)))

(defun llama--completion-url ()
  "Llama-cpp completion URL."
  (format "http://%s:%d/completion" llama-host llama-port))

(defun llama--request-body (prompt)
  "Llama-cpp POST request body for the PROMPT."
  (defvar url-http-method)
  (defvar url-http-proxy)
  (defvar url-http-target-url)
  (defvar url-http-referer)
  (defvar url-http-extra-headers)
  (defvar url-http-data)
  (let ((url-http-method "POST")
        (url-http-proxy nil)
        (url-http-target-url (url-generic-parse-url (llama--completion-url)))
        (url-http-referer nil)
        (url-http-extra-headers `(("Content-Type" . "application/json")))
        (url-http-data (json-serialize `(:prompt ,prompt :n_predict 2500 :stream t))))
    (url-http-create-request)))

(defun llama--process-filter (callback proc data)
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
                (while (string-match llama--rx s llama--start)
                  (setq llama--start (1+ (match-end 0)))
                  (let ((json (json-parse-string (substring s (match-beginning 1) (match-end 1)) :object-type 'plist)))
                    (funcall callback (plist-get json :content))))))))))))

(defun llama-complete (prompt callback)
  "Complete the PROMPT using llama-cpp server.
CALLBACK is called multiple times after a new token generated.

It cancels the previous running llama generation if any."
  (llama-cancel)
  (let* ((buffer (get-buffer-create llama--process-buffer))
         (process (make-network-process
                   :name llama--process
                   :buffer buffer
                   :host llama-host
                   :service llama-port
                   :filter (-partial #'llama--process-filter callback))))
    (process-send-string process (llama--request-body prompt))))

(provide 'llama)
;;; llama.el ends here

;; Local Variables:
;; End:
