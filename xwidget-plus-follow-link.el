;;; xwidget-plus-follow-link.el -- Link navigation in browsers -*- lexical-binding: t; -*-

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

(require 'xwidget-plus-common)

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

(defun xwidget-plus-follow-link-candidates ()
  "Return the currently selected link and the candidates.

The return value is a list whose first element is the selected
link and the rest are the candidates."
  (with-current-buffer (ivy-state-buffer ivy-last)
    (cons (ivy-state-current ivy-last) (ivy--filter ivy-text ivy--all-candidates))))

(defun xwidget-plus-follow-link-highlight (xwidget links)
  "Highligh LINKS in XWIDGET buffer when updating candidates."
  (let* ((candidates (xwidget-plus-follow-link-candidates))
         (selected (car candidates))
         (cands (cdr candidates))
         (cands-id (seq-filter (lambda (v) (seq-contains-p cands (cdr v))) links))
         (selected-id (car (rassoc selected links)))
         (script (--js "__xwidget_plus_follow_link_highlight('%s', '%s');" js-- (json-serialize cands-id) selected-id)))
    (xwidget-webkit-execute-script xwidget script)))

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

(defun xwidget-plus-follow-link-iv)
(defun xwidget-plus-follow-link-callback (links)
  "Ask for a link belonging to the alist LINKS."
  (let* ((xwidget (xwidget-webkit-current-session))
         (links (xwidget-plus-follow-link-prepare-links links))
         (choice (seq-map #'cdr links))
         link)
    (unwind-protect
        (xwidget-plus-follow-link-action xwidget links
                                         (ivy-read "Link: " choice :update-fn (apply-partially #'xwidget-plus-follow-link-highlight xwidget links)))
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

(provide 'xwidget-plus-follow-link)

;;; xwidget-plus-follow-link.el ends here

;; Local Variables:
;; eval: (mmm-mode)
;; eval: (mmm-add-classes '((elisp-js :submode js-mode :face mmm-code-submode-face :delimiter-mode nil :front "--js \"" :back "\" js--")))
;; mmm-classes: elisp-js
;; End:
