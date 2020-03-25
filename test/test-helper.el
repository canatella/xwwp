;;; test-helper.el -- Test helpers for xwidget-webkit-plus

;; Copyright (C) 2020 Damien Merenne <dam@cosinux.org>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(when (> emacs-major-version 26)
  (require 'debug)
  (defun ert--print-backtrace (frames)
    (insert (backtrace-to-string frames))))


(defconst xwwp-test-path (file-name-as-directory
                             (file-name-directory (or load-file-name buffer-file-name)))
  "The test directory.")

(defconst xwwp-test-data-path (file-name-as-directory
                                       (concat xwwp-test-path "data"))
  "The test data directory.")

(defconst xwwp-root-path (file-name-as-directory
                                         (file-name-directory
                                          (directory-file-name xwwp-test-path)))
  "The package root path.")

(add-to-list 'load-path xwwp-root-path)

(defun xwwp-event-dispatch (&optional seconds)
  (save-excursion
    (with-current-buffer (xwidget-buffer (xwidget-webkit-last-session))
      (let ((event (read-event nil nil seconds)))
        (when event
          (message "event:%s " event)
          (xwidget-event-handler))
        event))))

(defun xwwp-event-loop ()
  (save-excursion
    (with-current-buffer (xwidget-buffer (xwidget-webkit-last-session))
      (while (xwwp-event-dispatch 0.3)))))

(defmacro with-browse (file &rest body)
  (declare (indent 1))
  (let ((url (format "file://%s%s" (expand-file-name xwwp-test-data-path) file)))
    `(progn
       (xwidget-webkit-browse-url ,url)
       ;; this will trigger a loading event
       (xwwp-event-dispatch)
       (let ((xwidget (xwidget-webkit-last-session)))
         (xwwp-js-inject xwidget 'test)
         (xwwp-event-loop)
         (with-current-buffer (xwidget-buffer xwidget)
           ,@body)))))


(defun xwwp-wait-for (script)
  "Wait until scripts evaluate to true")
(provide 'test-helper)
;;; test-helper.el ends here
