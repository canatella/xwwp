;;; xwidget-plus.el --- Improve xwidget usability

;; Author: Damien Merenne
;; URL: https://github.com/canatella/xwidget-plus
;; Created: 2020-03-11
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "25.3") (ivy "0.13.0"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package augment the xwidget-webkit browser to make it more usable.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:


;;; User customizable variable

;;


;;; Code:
(require 'xwidget)
(require 'ivy)

(defgroup xwidget-plus nil
  "Augment the xwidget webkit browser."
  :group 'convenience)

(defcustom xwidget-plus-follow-link-candidate-style '(("border" . "1px dashed blue")
                                                      ("background" . "#0000ff20"))
  "Style to apply to candidate links."
  :type '(list (cons string string))
  :group 'xwidget-plus)

(defcustom xwidget-plus-follow-link-selected-style '(("border" . "1px dashed red")
                                                  ("background" . "#ff000020"))
  "Style to apply to currently selected link."
  :type '(list (cons string string))
  :group 'xwidget-plus)

;; Bring the window to front when summoning browse
(defun xwidget-plus-webkit-browse-url-advise (&rest _)
    "Advice to add switch to window when calling `xwidget-webkit-browse-url'."
    (switch-to-buffer-other-window (xwidget-buffer (xwidget-webkit-current-session))))
(advice-add #'xwidget-webkit-browse-url :after #'xwidget-plus-webkit-browse-url-advise)

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

(defconst xwidget-plus-follow-link-script (--js "
function __xwidget_plus_follow_link_cleanup() {
    document.querySelectorAll('a').forEach(a => {
        a.classList.remove('xwidget-plus-follow-link-candidate', 'xwidget-plus-follow-link-selected');
    });
}
function __xwidget_plus_follow_link_highlight(json, selected) {
    var ids = JSON.parse(json);
    document.querySelectorAll('a').forEach((a, id) => {
        a.classList.remove('xwidget-plus-follow-link-candidate', 'xwidget-plus-follow-link-selected');
        if (selected == id) {
            a.classList.add('xwidget-plus-follow-link-selected');
            a.scrollIntoView({behavior: 'smooth', block: 'center'});
        } else if (ids[id]) {
            a.classList.add('xwidget-plus-follow-link-candidate');
        }
    });
}
function __xwidget_plus_follow_link_action(id) {
    __xwidget_plus_follow_link_cleanup();
    document.querySelectorAll('a')[id].click();
}

function __xwidget_plus_follow_link_links() {
    var r = {};
    document.querySelectorAll('a').forEach((a, i) => {
        if (a.offsetWidth || a.offsetHeight || a.getClientRects().length)
            r[i] = a.innerText;
    });
    return r;
}
" js--))

(defun xwidget-plus-follow-link-highlight (xwidget links)
  "Highligh LINKS in XWIDGET buffer when updating ivy candidates."
  (with-current-buffer (ivy-state-buffer ivy-last)
    (let* ((cands (ivy--filter ivy-text ivy--all-candidates))
           (selected (ivy-state-current ivy-last))
           (cands-id (seq-filter (lambda (v) (seq-contains-p cands (cdr v))) links))
           (selected-id (car (rassoc selected links)))
           (script (--js "__xwidget_plus_follow_link_highlight('%s', '%s');" js-- (json-serialize cands-id) selected-id)))
      (xwidget-webkit-execute-script xwidget script))))

(defun xwidget-plus-follow-link-exit (xwidget)
  "Exit follow link mode in XWIDGET."
  (let ((script "__xwidget_plus_follow_link_cleanup();"))
    (xwidget-webkit-execute-script xwidget script)))

(defun xwidget-plus-follow-link-action (xwidget links selected)
  "Activate link matching SELECTED in XWIDGET LINKS."
  (let* ((selected-id (car (rassoc selected links)))
         (script (--js "__xwidget_plus_follow_link_action('%s');" js-- selected-id)))
    (xwidget-webkit-execute-script xwidget script)))

(defun xwidget-plus-follow-link-prepare-links (links)
  "Prepare the alist of LINKS."
  (setq links (seq-sort-by (lambda (v) (string-to-number (car v))) #'< links))
  (setq links (seq-map (lambda (v) (cons (intern (car v)) (xwidget-plus-follow-link-format-link (cdr v))))
                       links))
  (seq-filter #'identity links))

(defun xwidget-plus-follow-link-callback (links)
  "Ask for a link belonging to the alist LINKS."
  (let* ((xwidget (xwidget-webkit-current-session))
         (links (xwidget-plus-follow-link-prepare-links links))
         (choice (seq-map #'cdr links)))
    (unwind-protect
        (ivy-read "Link: " choice
                  :action (apply-partially #'xwidget-plus-follow-link-action xwidget links)
                  :update-fn (apply-partially #'xwidget-plus-follow-link-highlight xwidget links))
      (xwidget-plus-follow-link-exit xwidget))))

;;;###autoload
(defun xwidget-plus-follow-link (&optional xwidget)
  "Ask for a link in the XWIDGET session or the current one and follow it."
  (interactive)
  (let ((xwidget (or xwidget (xwidget-webkit-current-session)))
        (script (--js "__xwidget_plus_follow_link_links();" js--)))
    (xwidget-plus-inject-style xwidget "__xwidget_plus_follow_link_style" (xwidget-plus-follow-link-style-definition))
    (xwidget-plus-inject-script xwidget "__xwidget_plus_follow_link_script" xwidget-plus-follow-link-script)
    (xwidget-webkit-execute-script xwidget script #'xwidget-plus-follow-link-callback)))

(provide 'xwidget-link)

;;; xwidget-link.el ends here

;; Local Variables:
;; eval: (mmm-mode)
;; eval: (mmm-add-classes '((elisp-js :submode js-mode :face mmm-code-submode-face :delimiter-mode nil :front "--js \"" :back "\" js--")))
;; mmm-classes: elisp-js
;; End:
