;;; xwwp-follow-link-ivy.el --- Link navigation in `xwidget-webkit' sessions using `ivy' -*- lexical-binding: t; -*-

;; Author: Damien Merenne
;; URL: https://github.com/canatella/xwwp
;; Created: 2020-03-11
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (xwwp "0.1"))

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
;; `ivy' completion.

;;; Code:

(require 'xwwp-follow-link)
(require 'ivy)

(defclass xwwp-follow-link-completion-backend-ivy (xwwp-follow-link-completion-backend) ())

(cl-defmethod xwwp-follow-link-candidates ((_ xwwp-follow-link-completion-backend-ivy))
  (with-current-buffer (ivy-state-buffer ivy-last)
    (let* ((collection (ivy-state-collection ivy-last))
           (current (ivy-state-current ivy-last))
           (candidates (ivy--filter ivy-text ivy--all-candidates))
           (result (cons current candidates)))
      (seq-map (lambda (c) (cdr (nth (get-text-property 0 'idx c) collection))) result))))

(cl-defmethod xwwp-follow-link-read ((_ xwwp-follow-link-completion-backend-ivy) prompt collection action update-fn)
  (ivy-read prompt collection :require-match t :action (lambda (v) (funcall action (cdr v))) :update-fn update-fn))

(provide 'xwwp-follow-link-ivy)
;;; xwwp-follow-link-ivy.el ends here
