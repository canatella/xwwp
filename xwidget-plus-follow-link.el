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

(require 'xwidget)
(require 'xwidget-plus-common)
(require 'ivy)
(require 'eieio)

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

(defun xwidget-plus-follow-link-style-definition ()
  "Return the css definitions for the follow link feature."
  (concat (xwidget-plus-make-class "xwidget-plus-follow-link-candidate" xwidget-plus-follow-link-candidate-style)
          (xwidget-plus-make-class "xwidget-plus-follow-link-selected" xwidget-plus-follow-link-selected-style)))


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
        } else if (ids.includes(id)) {
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
        if (a.offsetWidth || a.offsetHeight || a.getClientRects().length) {
            if (a.innerText.match(/\\\\S/))
                r[i] = a.innerText;
        }
    });
    return r;
}
" js--))


;; Completion backend class
(defclass xwidget-plus-completion-backend () ((collection) (text)))

(cl-defmethod xwidget-plus-follow-link-candidates ((backend xwidget-plus-completion-backend))
    "Return the BACKEND selected link and the candidates.

The return value is a list whose first element is the selected id
link and the rest are the candidates ids.

Return nil if the backend does not support narrowing selection list.")

(cl-defmethod xwidget-plus-follow-link-read ((backend xwidget-plus-completion-backend) prompt collection action update-fn)
  "use BACKEND to PROMPT the user for a link in COLLECTION.

ACTION should be called with the resulting link.

UPDATE-FN is a function that can be called when the candidates
list is narrowed. It will highlight the link list in the
browser.")


;; Default backend using completing-read
(defclass xwidget-plus-completion-backend-default (xwidget-plus-completion-backend) ())

(cl-defmethod xwidget-plus-follow-link-candidates ((backend xwidget-plus-completion-backend-default))
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

(cl-defmethod xwidget-plus-follow-link-read ((backend xwidget-plus-completion-backend-default) prompt collection action update-fn)
  "use BACKEND to PROMPT the user for a link in COLLECTION.

ACTION should be called with the resulting link.

UPDATE-FN is a function that can be called when the candidates
list is narrowed. It will highlight the link list in the
browser."
  (funcall action (cdr (assoc (completing-read prompt (lambda (str pred _)
                                                        (oset backend text str)
                                                        (funcall update-fn)
                                                        (try-completion str collection pred))
                                               nil t)
                              collection))))


;; Ido backend using ido-completing-read
(defclass xwidget-plus-completion-backend-ido (xwidget-plus-completion-backend) ())

(cl-defmethod xwidget-plus-follow-link-candidates ((backend xwidget-plus-completion-backend-ido))
  (let ((collection (oref backend collection)))
    (when collection
      (seq-map (lambda (i) (cdr (assoc i collection))) ido-matches))))

(cl-defmethod xwidget-plus-follow-link-read ((backend xwidget-plus-completion-backend-ido) prompt collection action update-fn)
  (let ((choices (seq-map #'car collection)))
    (advice-add #'ido-set-matches :after update-fn)
    (let ((link (cdr (assoc (ido-completing-read prompt choices nil t) collection))))
      (oset backend collection nil)
      (advice-remove #'ido-set-matches #'update-fn)
      (funcall action link))))


;; Ivy backend using completing read
(defclass xwidget-plus-completion-backend-ivy (xwidget-plus-completion-backend) ())

(cl-defmethod xwidget-plus-follow-link-candidates ((backend xwidget-plus-completion-backend-ivy))
  (with-current-buffer (ivy-state-buffer ivy-last)
    (let* ((collection (ivy-state-collection ivy-last))
           (current (ivy-state-current ivy-last))
           (candidates (ivy--filter ivy-text ivy--all-candidates))
           (result (cons current candidates)))
      (seq-map (lambda (c) (cdr (nth (get-text-property 0 'idx c) collection))) result))))

(cl-defmethod xwidget-plus-follow-link-read ((backend xwidget-plus-completion-backend-ivy) prompt collection action update-fn)
  (ivy-read "Link: " collection :require-match t :action (lambda (v) (funcall action (cdr v))) :update-fn update-fn))


;; Helm backend
(defclass xwidget-plus-completion-backend-helm (xwidget-plus-completion-backend) ((candidates)))

(cl-defmethod xwidget-plus-follow-link-candidates ((backend xwidget-plus-completion-backend-helm))
  (let* ((candidates (oref backend candidates))
         (selection (helm-get-selection))
         (selected (when selection (cdr (elt (oref backend collection) selection))))
         (result (seq-map #'cdr candidates)))
    (cons selected result)))

(cl-defmethod xwidget-plus-follow-link-read ((backend xwidget-plus-completion-backend-helm) prompt collection action update-fn)
  (add-hook 'helm-after-initialize-hook (lambda ()
                                          (with-current-buffer "*helm-xwidget-plus*"
                                            (add-hook 'helm-move-selection-after-hook update-fn nil t)))
            nil t)
  (helm :sources
        (helm-make-source "Xwidget Plus" 'helm-source-sync
          :candidates collection
          :action action
          :filtered-candidate-transformer (lambda (candidates _)
                                            (oset backend candidates candidates)
                                            (funcall update-fn)
                                            candidates))
        :prompt prompt
        :buffer "*helm-xwidget-plus*"))

(defvar xwidget-plus-completion-backend-instance (xwidget-plus-completion-backend))

(defun xwidget-plus-follow-link-highlight (xwidget)
  "Highligh LINKS in XWIDGET buffer when updating candidates."
  (let ((links (xwidget-plus-follow-link-candidates xwidget-plus-completion-backend-instance)))
    (when links
      (let* ((selected (car links))
             (candidates (cdr links))
             (script (--js "__xwidget_plus_follow_link_highlight('%s', %s);" js-- (json-serialize (vconcat candidates)) (or selected "null"))))
        (xwidget-webkit-execute-script xwidget script)))))

(defun xwidget-plus-follow-link-exit (xwidget)
  "Exit follow link mode in XWIDGET."
  (let ((script "__xwidget_plus_follow_link_cleanup();"))
    (xwidget-webkit-execute-script xwidget script)))

(defun xwidget-plus-follow-link-action (xwidget selected)
  "Activate link matching SELECTED in XWIDGET LINKS."
  (let ((script (--js "__xwidget_plus_follow_link_action(%s);" js-- selected)))
    (xwidget-webkit-execute-script xwidget script)))

(defun xwidget-plus-follow-link-format-link (str)
  "Format link title STR."
  (setq str (replace-regexp-in-string "^[[:space:][:cntrl:]]+" "" str))
  (setq str (replace-regexp-in-string "[[:space:][:cntrl:]]+$" "" str))
  (setq str (replace-regexp-in-string "[[:cntrl:]]+" "/" str))
  (replace-regexp-in-string "[[:space:]]+" " " str))

(defun xwidget-plus-follow-link-prepare-links (links)
  "Prepare the alist of LINKS."
  (seq-sort-by (lambda (v) (cdr v)) #'<
               (seq-map (lambda (v) (cons (xwidget-plus-follow-link-format-link (cdr v)) (string-to-number (car v))))
                        links)))

(defun xwidget-plus-follow-link-callback (links)
  "Ask for a link belonging to the alist LINKS."
  (let* ((xwidget (xwidget-webkit-current-session))
         (links (xwidget-plus-follow-link-prepare-links links))
         link)
    (oset xwidget-plus-completion-backend-instance collection links)
    (unwind-protect
        (condition-case nil
            (xwidget-plus-follow-link-read xwidget-plus-completion-backend-instance
                                           "Link: " links
                                           (apply-partially #'xwidget-plus-follow-link-action xwidget)
                                           (apply-partially #'xwidget-plus-follow-link-highlight xwidget))
          (quit (xwidget-plus-follow-link-exit xwidget))))
    (oset xwidget-plus-completion-backend-instance collection nil)))

;;;###autoload
(defun xwidget-plus-follow-link (&optional xwidget)
  "Ask for a link in the XWIDGET session or the current one and follow it."
  (interactive)
  (let ((xwidget (or xwidget (xwidget-webkit-current-session)))
        (script (--js "__xwidget_plus_follow_link_links();" js--)))
    (xwidget-plus-inject-style xwidget "__xwidget_plus_follow_link_style" (xwidget-plus-follow-link-style-definition))
    (xwidget-plus-inject-script xwidget "__xwidget_plus_follow_link_script" xwidget-plus-follow-link-script)
    (xwidget-webkit-execute-script xwidget script #'xwidget-plus-follow-link-callback)))

;; Local Variables:
;; eval: (mmm-mode)
;; eval: (mmm-add-classes '((elisp-js :submode js-mode :face mmm-code-submode-face :delimiter-mode nil :front "--js \"" :back "\" js--")))
;; mmm-classes: elisp-js
;; End:

(provide 'xwidget-plus-follow-link)
;;; xwidget-plus-follow-link.el ends here
