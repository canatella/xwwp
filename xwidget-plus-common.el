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

(defgroup xwidget-plus nil
  "Augment the xwidget webkit browser."
  :group 'convenience)

(defcustom xwidget-plus-completion-system 'default
  "The completion system to be used by xwidget plus.

Custom function should be a function that takes no arguments and
returns an instance of an eieio class extending
`xwidget-plus-completion-backend'."
  :group 'xwidget-plus
  :type '(radio
          (const :tag "Ido" ido)
          (const :tag "Helm" helm)
          (const :tag "Ivy" ivy)
          (const :tag "Default" default)
          (function :tag "Custom function")))

(require 'xwidget)
(require 'json)

(defun xwidget-plus-make-class (class style)
  "Generate a css CLASS definition from the STYLE alist."
  (format ".%s { %s }\\n" class (mapconcat (lambda (v) (format "%s: %s;" (car v) (cdr v))) style " ")))

(defmacro --js (js _ &rest replacements)
  "Apply `format' on JS with REPLACEMENTS  providing MMM mode delimiters.

This file has basic support for javascript using MMM mode and
local variables (see at the end of the file)."
  (declare (indent 2))
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
null;
" js-- id tag type content)))
    (xwidget-webkit-execute-script xwidget script)))

(defun xwidget-plus-inject-script (xwidget id script)
  "Inject javascript SCRIPT in XWIDGET session using a script element with ID."
  (xwidget-plus-inject-head-element xwidget "script" id "text/javascript" script))

(defun xwidget-plus-inject-style (xwidget id style)
  "Inject css STYLE in XWIDGET session using a style element with ID."
  (xwidget-plus-inject-head-element xwidget "style" id "text/css" style))

(defun xwidget-plus-lisp-to-js (identifier)
  "Convert IDENTIFIER from Lisp style to javascript style."
  (replace-regexp-in-string "-" "_" (if (symbolp identifier) (symbol-name identifier) identifier)))

(defvar xwidget-plus-js-scripts '() "An  alist of list of javascript function.")

(defun xwidget-plus-js-register-function (ns-name name js-script)
  "Register javascript function NAME in namespace NS-NAME with body JS-SCRIPT."
  (let* ((namespace (assoc ns-name xwidget-plus-js-scripts))
         (fun (when namespace (assoc name (cdr namespace)))))
    (cond (fun
           (delete fun namespace)
           (xwidget-plus-js-register-function ns-name name js-script))
          ((not namespace)
           (push (cons ns-name '()) xwidget-plus-js-scripts)
           (xwidget-plus-js-register-function ns-name name js-script))
          (t
           (push (cons name js-script) (cdr namespace))))
    (cons ns-name name)))

(defun xwidget-plus-js-funcall (xwidget namespace name &rest arguments)
  "Invoke javascript FUNCTION in XWIDGET instance passing ARGUMENTS witch CALLBACK in NAMESPACE."
  ;;; Try to be smart
  (let* ((json-args (seq-map #'json-encode arguments))
         (arg-string (string-join json-args ", "))
         (namespace (xwidget-plus-lisp-to-js namespace))
         (name (xwidget-plus-lisp-to-js name))
         (callback (let ((cb (car (last arguments)))) (when (functionp cb) cb)))
         (script (format "__xwidget_plus_%s_%s(%s)" namespace name arg-string)))
    (xwidget-webkit-execute-script xwidget script callback)))

(defmacro xwidget-plus-js-def (namespace name arguments docstring js-body)
  "Create a function NAME with ARGUMENTS, DOCSTRING and JS-BODY.

This will define a javascript function in the namespace NAMESPACE
and a Lisp function to call it. "
  (declare (indent 3) (doc-string 4))
  (let* ((js-arguments (seq-map #'xwidget-plus-lisp-to-js arguments))
         (js-name (xwidget-plus-lisp-to-js name))
         (js-namespace (xwidget-plus-lisp-to-js namespace))
         (lisp-arguments (append '(xwidget) arguments '(&optional callback)))
         (script (--js "function __xwidget_plus_%s_%s(%s) {%s};" js--
                   js-namespace js-name (string-join js-arguments ", ") (eval js-body)))
         (lisp-def  `(defun ,(intern (format "xwidget-plus-%s-%s" namespace name)) ,lisp-arguments
                       ,docstring
                       (xwidget-plus-js-funcall xwidget (quote ,namespace) (quote ,name) ,@arguments callback)))
         (lisp-store `(xwidget-plus-js-register-function (quote ,namespace) (quote ,name) ,script)))
    `(progn ,lisp-def ,lisp-store)))

(defun xwidget-plus-js-inject (xwidget ns-name)
  (let* ((namespace (assoc ns-name xwidget-plus-js-scripts))
         (script (mapconcat #'cdr (cdr namespace) "\n")))
    (xwidget-plus-inject-script xwidget (format "--xwidget-plus-%s" (symbol-name ns-name)) script)))

;; Local Variables:
;; eval: (mmm-mode)
;; eval: (mmm-add-group 'elisp-js '((elisp-rawjs :submode js-mode
;;                                               :face mmm-code-submode-face
;;                                               :delimiter-mode nil
;;                                               :front "--js \"" :back "\" js--")
;;                                  (elisp-defjs :submode js-mode
;;                                               :face mmm-code-submode-face
;;                                               :delimiter-mode nil
;;                                               :front "xwidget-plus-defjs .*\n.*\"\"\n" :back "\")\n")))
;; mmm-classes: elisp-js
;; End:

(provide 'xwidget-plus-common)
;;; xwidget-plus-common.el ends here
