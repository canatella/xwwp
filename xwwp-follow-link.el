;;; xwwp-follow-link.el --- Link navigation in `xwidget-webkit' sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damien Merenne <dam@cosinux.org>, Q. Hong <qhong@mit.edu>

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
;; minibuffer completion.

;;; Code:

(require 'xwidget)
(require 'xwwp)
(require 'eieio)
(require 'cl-lib)
(require 'ido)

(defgroup xwwp-follow-link nil
  "`xwidget-webkit' follow link customizations."
  :group 'xwwp)

(defcustom xwwp-follow-link-completion-system 'default
  "The completion system to be used by xwidget plus.

Custom function should be a function that takes no arguments and
returns an instance of an eieio class extending
`xwwp-follow-link-completion-backend'."
  :group 'xwwp-follow-link
  :type '(radio
          (const :tag "Ido" ido)
          (const :tag "Helm" helm)
          (const :tag "Ivy" ivy)
          (const :tag "Default" default)
          (function :tag "Custom function")))

(defcustom xwwp-follow-link-candidate-style '(("border" . "1px dashed blue")
                                              ("background" . "#0000ff20"))
  "Style to apply to candidate links."
  :type '(list (cons string string))
  :group 'xwwp-follow-link)

(defcustom xwwp-follow-link-selected-style '(("border" . "1px dashed red")
                                             ("background" . "#ff000020"))
  "Style to apply to currently selected link."
  :type '(list (cons string string))
  :group 'xwwp-follow-link)

(defun xwwp-follow-link-style-definition ()
  "Return the css definitions for the follow link feature."
  (concat (xwwp-css-make-class "xwwp-follow-link-candidate" xwwp-follow-link-candidate-style)
          (xwwp-css-make-class "xwwp-follow-link-selected" xwwp-follow-link-selected-style)))

(xwwp-js-def follow-link cleanup ()
  "Remove all custom class from links.""
window.__xwidget_plus_follow_link_candidates.forEach(a => {
    a.classList.remove('xwwp-follow-link-candidate', 'xwwp-follow-link-selected');
});
window.__xwidget_plus_follow_link_candidates = null;
")

(xwwp-js-def follow-link highlight (ids selected)
  "Highlight IDS as candidate and SELECTED as selected.""
window.__xwidget_plus_follow_link_candidates.forEach((a, id) => {
    a.classList.remove('xwwp-follow-link-candidate', 'xwwp-follow-link-selected');
    if (selected == id) {
        a.classList.add('xwwp-follow-link-selected');
        a.scrollIntoView({behavior: 'smooth', block: 'center'});
    } else if (ids && ids.includes(id)) {
        a.classList.add('xwwp-follow-link-candidate');
    }
});
")

(xwwp-js-def follow-link action (link-id)
  "Click on the link identified by LINK-ID""
let selected = window.__xwidget_plus_follow_link_candidates[link_id];
__xwidget_plus_follow_link_cleanup();
selected.click();
")

(xwwp-js-def follow-link fetch-links ()
  "Fetch all visible, non empty links from the current page.""
var r = {};
window.__xwidget_plus_follow_link_candidates = Array.from(document.querySelectorAll('a'));
window.__xwidget_plus_follow_link_candidates.forEach((a, i) => {
    if (a.offsetWidth || a.offsetHeight || a.getClientRects().length) {
        if (a.innerText.match(/\\\\S/))
            r[i] = [a.innerText, a.href];
    }
});
return r;
")


;; Completion backend class
(defclass xwwp-follow-link-completion-backend () ((collection) (text)))

(cl-defmethod xwwp-follow-link-candidates ((_backend xwwp-follow-link-completion-backend))
  "Return the BACKEND selected link and the candidates.

The return value is a list whose first element is the selected id
link and the rest are the candidates ids.

Return nil if the backend does not support narrowing selection list.")

(cl-defmethod xwwp-follow-link-read ((_backend xwwp-follow-link-completion-backend)
                                     _prompt _collection _action _update-fn)
  "Use BACKEND to PROMPT the user for a link in COLLECTION.

ACTION should be called with the resulting link.

UPDATE-FN is a function that can be called when the candidates
list is narrowed.It will highlight the link list in the
browser.")


;; Default backend using completing-read
(defclass xwwp-follow-link-completion-backend-default (xwwp-follow-link-completion-backend) ())

(cl-defmethod xwwp-follow-link-candidates ((backend xwwp-follow-link-completion-backend-default))
  "Return the BACKEND selected link and the candidates.

The return value is a list whose first element is the selected id
link and the rest are the candidates ids.

Return nil if the backend does not support narrowing selection list."
  (let* ((collection (oref backend collection))
         (text (oref backend text))
         (matches (seq-filter (lambda (i) (string-match-p (concat "^" (regexp-quote text)) (car i))) collection))
         (matches (seq-map #'cdr matches)))
    (if (= 1 (length matches))
        matches
      (cons nil matches))))

(cl-defmethod xwwp-follow-link-read ((backend xwwp-follow-link-completion-backend-default) prompt collection action update-fn)
  "Use BACKEND to PROMPT the user for a link in COLLECTION.

ACTION should be called with the resulting link.

UPDATE-FN is a function that can be called when the candidates
list is narrowed.It will highlight the link list in the
browser."
  (funcall action (cdr (assoc (completing-read prompt (lambda (str pred flag)
                                                        (oset backend text str)
                                                        (funcall update-fn)
                                                        (complete-with-action flag collection str pred))
                                               nil t)
                              collection))))

(declare-function xwwp-follow-link-completion-backend-ido "xwwp-follow-link-ido")
(declare-function xwwp-follow-link-completion-backend-ivy "xwwp-follow-link-ivy")
(declare-function xwwp-follow-link-completion-backend-helm "xwwp-follow-link-helm")

(defun xwwp-follow-link-make-backend ()
  "Instanciate a completion backend."
  (cond ((eq xwwp-follow-link-completion-system 'default)
         #'xwwp-follow-link-completion-backend-default)
        ((eq xwwp-follow-link-completion-system 'ivy)
         (unless (require 'xwwp-follow-link-ivy nil t)
           (user-error "Install the `xwwp-follow-link-ivy' package to use `xwwp-follow-link' with `ivy'"))
         #'xwwp-follow-link-completion-backend-ivy)
        ((eq xwwp-follow-link-completion-system 'helm)
         (unless (require 'xwwp-follow-link-helm nil t)
           (user-error "Install the `xwwp-follow-link-helm' package to use `xwwp-follow-link' with `helm'"))
         #'xwwp-follow-link-completion-backend-helm)
        ((eq xwwp-follow-link-completion-system 'ido)
         (require 'xwwp-follow-link-ido)
         #'xwwp-follow-link-completion-backend-ido)
        ((eq xwwp-follow-link-completion-system 'default)
         #'xwwp-follow-link-completion-backend-default)
        (t xwwp-follow-link-completion-system)))


(defvar xwwp-follow-link-completion-backend-instance '())

(defun xwwp-follow-link-update (xwidget)
  "Highlight LINKS in XWIDGET buffer when updating candidates."
  (let ((links (xwwp-follow-link-candidates xwwp-follow-link-completion-backend-instance)))
    (when links
      (let* ((selected (car links))
             (candidates (cdr links)))
        (xwwp-follow-link-highlight xwidget (mapcar 'car candidates) (car selected))))))

(defun xwwp-follow-link-trigger-action (xwidget selected)
  "Activate link matching SELECTED in XWIDGET LINKS.
The SELECTED value is the cdr of an assoc in the collection passed to
completion back end, which is of the form (numerical-id link-url)"
  (xwwp-follow-link-action xwidget (car selected)))

(defun xwwp-follow-link-format-link (str)
  "Format link title STR."
  (setq str (replace-regexp-in-string "^[[:space:][:cntrl:]]+" "" str))
  (setq str (replace-regexp-in-string "[[:space:][:cntrl:]]+$" "" str))
  (setq str (replace-regexp-in-string "[[:cntrl:]]+" "/" str))
  (replace-regexp-in-string "[[:space:]]+" " " str))

(defun xwwp-follow-link-prepare-links (links)
  "Prepare the alist of LINKS."
  (seq-sort-by (lambda (v) (cadr v)) #'<
               (seq-map (lambda (v) (list (xwwp-follow-link-format-link (aref (cdr v) 0))
                                          (string-to-number (car v))
                                          (aref (cdr v) 1)))
                        links)))

(defun xwwp-follow-link-callback (links)
  "Ask for a link belonging to the alist LINKS.
LINKS maps a numerical ID to an array of form [link-text, link-uri]"
  (let* ((xwidget (xwidget-webkit-current-session))
         (links (xwwp-follow-link-prepare-links links)))
    (oset xwwp-follow-link-completion-backend-instance collection links)
    (condition-case nil
        (xwwp-follow-link-read xwwp-follow-link-completion-backend-instance
                               "Link: " links
                               (apply-partially #'xwwp-follow-link-trigger-action xwidget)
                               (apply-partially #'xwwp-follow-link-update xwidget))
      (quit (xwwp-follow-link-cleanup xwidget)))
    (oset xwwp-follow-link-completion-backend-instance collection nil)))

;;;###autoload
(defun xwwp-follow-link (&optional xwidget)
  "Ask for a link in the XWIDGET session or the current one and follow it."
  (interactive)
  (setq xwwp-follow-link-completion-backend-instance (funcall (xwwp-follow-link-make-backend)))
  (let ((xwidget (or xwidget (xwidget-webkit-current-session))))
    (xwwp-html-inject-style xwidget "__xwidget_plus_follow_link_style" (xwwp-follow-link-style-definition))
    (xwwp-js-inject xwidget 'follow-link)
    (xwwp-follow-link-fetch-links xwidget #'xwwp-follow-link-callback)))

;; Local Variables:
;; eval: (mmm-mode)
;; eval: (mmm-add-group 'elisp-js '((elisp-rawjs :submode js-mode
;;                                               :face mmm-code-submode-face
;;                                               :delimiter-mode nil
;;                                               :front "xwwp--js \"" :back "\" js--")
;;                                  (elisp-defjs :submode js-mode
;;                                               :face mmm-code-submode-face
;;                                               :delimiter-mode nil
;;                                               :front "xwwp-js-def .*\n.*\"\"\n" :back "\")\n")))
;; mmm-classes: elisp-js
;; End:

(provide 'xwwp-follow-link)
;;; xwwp-follow-link.el ends here
