;;; xwwp-follow-link-ido.el --- Link navigation in `xwidget-webkit' sessions using `ido' -*- lexical-binding: t; -*-

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
;; `ido' completion.

;;; Code:

(require 'xwwp-follow-link)
(require 'ido)

(defclass xwwp-follow-link-completion-backend-ido (xwwp-follow-link-completion-backend) ())

(cl-defmethod xwwp-follow-link-candidates ((backend xwwp-follow-link-completion-backend-ido))
  (let ((collection (oref backend collection)))
    (when collection
      (seq-map (lambda (i) (cdr (assoc i collection))) ido-matches))))

(cl-defmethod xwwp-follow-link-read ((backend xwwp-follow-link-completion-backend-ido) prompt collection action update-fn)
  (let ((choices (seq-map #'car collection)))
    (advice-add #'ido-set-matches :after update-fn)
    (let ((link (unwind-protect
                    (cdr (assoc (ido-completing-read prompt choices nil t) collection))
                  (oset backend collection nil)
                  (advice-remove #'ido-set-matches update-fn))))
      (funcall action link))))

(provide 'xwwp-follow-link-ido)
;;; xwwp-follow-link-ido.el ends here
