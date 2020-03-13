;;; xwidget-plus-common.el -- Helper functions for xwidget-plus.

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

(require 'xwidget)

(defun xwidget-plus-make-class (class style)
  "Generate a css CLASS definition from the STYLE alist."
  (format ".%s { %s }\\n" class (mapconcat (lambda (v) (format "%s: %s;" (car v) (cdr v))) style " ")))

(defun xwidget-plus-follow-link-style-definition ()
  "Return the css definitions for the follow link feature."
  (concat (xwidget-plus-make-class "xwidget-plus-follow-link-candidate" xwidget-plus-follow-link-candidate-style)
          (xwidget-plus-make-class "xwidget-plus-follow-link-selected" xwidget-plus-follow-link-selected-style)))

(defun xwidget-plus-follow-link-format-link (str)
  "Format link title STR."
  (setq str (replace-regexp-in-string "^[[:space:][:cntrl:]]+" "" str))
  (setq str (replace-regexp-in-string "[[:space:][:cntrl:]]+$" "" str))
  (setq str (replace-regexp-in-string "[[:cntrl:]]+" "/" str))
  (replace-regexp-in-string "[[:space:]]+" " " str))

(defmacro --js (js _ &rest replacements)
  "Apply `format' on JS with REPLACEMENTS  providing MMM mode delimiters.

This file has basic support for javascript using MMM mode and
local variables (see at the end of the file)."
  (declare (indent 3))
  `(format ,js ,@replacements))

(defun xwidget-plus-js-string-escape (string)
  "Escape STRING for injection."
  (replace-regexp-in-string "\n" "\\\\n" (replace-regexp-in-string "'" "\\\\'" string)))

(defun xwidget-plus-inject-head-element (xwidget tag id type content)
  "Insert TAG element under XWIDGET head with ID TYPE and CONTENT."
  (let* ((id (xwidget-plus-js-string-escape id))
         (tag (xwidget-plus-js-string-escape tag))
         (type (xwidget-plus-js-string-escape type))
         (content (xwidget-plus-js-string-escape content))
         (script (--js "
__xwidget_id = '%s';
if (!document.getElementById(__xwidget_id)) {
    var e = document.createElement('%s');
    e.type = '%s';
    e.id = __xwidget_id;
    e.innerHTML = '%s';
    document.getElementsByTagName('head')[0].appendChild(e);
};
" js-- id tag type content)))
    (xwidget-webkit-execute-script xwidget script)))

(defun xwidget-plus-inject-script (xwidget id script)
  "Inject javascript SCRIPT in XWIDGET session using a script element with ID."
  (xwidget-plus-inject-head-element xwidget "script" id "text/javascript" script))

(defun xwidget-plus-inject-style (xwidget id style)
  "Inject css STYLE in XWIDGET session using a style element with ID."
  (xwidget-plus-inject-head-element xwidget "style" id "text/css" style))


(provide 'xwidget-plus-common)

;;; xwidget-plus-common.el ends here
;; Local Variables:
;; eval: (mmm-mode)
;; eval: (mmm-add-classes '((elisp-js :submode js-mode :face mmm-code-submode-face :delimiter-mode nil :front "--js \"" :back "\" js--")))
;; mmm-classes: elisp-js
;; End:
