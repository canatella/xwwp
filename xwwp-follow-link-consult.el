;;; xwwp-follow-link-consult.el --- Link navigation in `xwidget-webkit' sessions using `consult' -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damien Merenne <dam@cosinux.org>

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
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

;; Add support for navigating web pages in `xwidget-webkit' sessions using the
;; `consult' completion.

;;; Code:

(require 'xwwp-follow-link)
(require 'consult)

(defvar xwwp-follow-link-consult-candidates nil "List of candidates for the active completion")

(defvar xwwp-follow-link-consult-history
  nil
  "Stores history for links selected with command `xwwp-follow-link-completion-backend-consult'.")

(defun consult-xwwp-follow-link--preview (candidates update-fn)
  "Create preview function for xwwp links CANDIDATES by calling UPDATE-FN, restore POINT when done."
  (lambda (cand restore)
    (if restore nil
      (let* ((selected cand)
             (rest (seq-filter (lambda (c) (not (eq c selected))) candidates))
             (xwwp-follow-link-consult-candidates (cons selected rest)))
        (funcall update-fn)))))

(defun xwwp-follow-link-consult-lookup (_ candidates cand)
  "Lookup for cand in candidates."
  (seq-find (lambda (c) (string= (car c) cand)) candidates))

(defclass xwwp-follow-link-completion-backend-consult (xwwp-follow-link-completion-backend) ())

(cl-defmethod xwwp-follow-link-candidates ((_ xwwp-follow-link-completion-backend-consult))
  "Follow a link selected with consult."
  (seq-map (lambda (c) `(,(cadr c) ,(caddr c))) xwwp-follow-link-consult-candidates))

(cl-defmethod xwwp-follow-link-read ((_ xwwp-follow-link-completion-backend-consult) prompt collection action update-fn)
  "Select a link from COLLECTION a with consult showing PROMPT using UPDATE-FN to highlight the link in the xwidget session and execute ACTION once a link has been select."
  (funcall action (cdr (consult--read collection
                                      :prompt prompt
                                      :state (consult-xwwp-follow-link--preview collection update-fn)
                                      :require-match t
                                      :sort nil
                                      :lookup #'xwwp-follow-link-consult-lookup
                                      :history 'xwwp-follow-link-consult-history))))

(provide 'xwwp-follow-link-consult)
;;; xwwp-follow-link-consult.el ends here
