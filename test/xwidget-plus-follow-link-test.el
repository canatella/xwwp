;;; xwidget-plus-follow-link-test.el -- xwwp follow link test suite -*- lexical-binding: t; -*-

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

;; Test suite for xwidget-webkit-plus-follow-link feature

;;; Code:

(require 'test-helper)
(require 'with-simulated-input)
(require 'ivy)
(require 'helm)
(require 'xwidget-plus-follow-link)

;; Disable helm and ivy, otherwise they hijack completing-read and break the
;; default completion backend test.
(setq completing-read-function #'completing-read-default)


;; A mocked backend class that doesn't interactively read anything.
(defclass xwidget-plus-completion-backend-test (xwidget-plus-completion-backend)
  ((candidates-mock :initarg :candidates-mock)
   (selected-mock :initarg :selected-mock)
   (action-fn)
   (classes)))

(defun xwidget-plus-update-fn-callback (result)
  "Called after updating candidates with the css classes in RESULT."
  (let ((backend xwidget-plus-completion-backend-instance))
    ;; Store the results.
    (oset backend classes result)
    ;; Trigger the action function with the mocked selected link
    (funcall (oref backend action-fn) (oref backend selected-mock))
    (xwidget-plus-event-dispatch)))

(cl-defmethod xwidget-plus-follow-link-candidates ((backend xwidget-plus-completion-backend-test))
  "Return the list of BACKEND mocked candidates."
  (oref backend candidates-mock))

(cl-defmethod xwidget-plus-follow-link-read ((backend xwidget-plus-completion-backend-test) _ _ action-fn update-fn)
  "Store ACTION-FN in BACKEND, call the UPDATE-FN, and fetch the link element classes."
  ;; Stoire action so that we can call it after having fetch the css classes.
  (oset backend action-fn action-fn)
  ;; Trigger the javascript update.
  (funcall update-fn)
  ;; Fetch css classes.
  (xwidget-webkit-execute-script (xwidget-webkit-current-session) (--js "
map = Array.prototype.map;
r = {};
document.querySelectorAll('a').forEach(l => {
    r[l.id] = map.call(l.classList, t => {
        return t.toString();
     });
});
r
" js--) #'xwidget-plus-update-fn-callback)
  (xwidget-plus-event-dispatch))

(cl-defmethod backend-test-link-classes ((backend xwidget-plus-completion-backend-test) link-id)
  "Return test BACKEND css class names for LINK-ID."
  (cdr (assoc link-id (oref backend classes))))

(defmacro with-backend (backend &rest body)
  "Run BODY with the specified BACKEND."
  (declare (indent 1))
  `(let* ((backend (,(intern (concat "xwidget-plus-completion-backend-" (symbol-name backend)))))
          (xwidget-plus-completion-backend-instance backend))
     ,@body))

(defmacro with-test-backend-browse (candidates selected url &rest body)
  "Run BODY with the specified BACKEND mocking CANDIDATES and SELECTED while browsing URL."
  (declare (indent 3))
  `(let* ((backend (xwidget-plus-completion-backend-test :candidates-mock ,candidates
                                                         :selected-mock ,selected))
          (xwidget-plus-completion-backend-instance backend))
     (with-browse ,url
       ,@body)))

(ert-deftest test-xwidget-plus-follow-link-prepare-links ()
  (let ((links '(("3" . "Functions")
                 ("1" . "Function Cells")
                 ("12" . "Structures")
                 ("2" . "Anonymous Functions")
                 ("9" . "Declare Form"))))
    (should (equal '(("Function Cells" . 1)
	             ("Anonymous Functions" . 2)
	             ("Functions" . 3)
	             ("Declare Form" . 9)
	             ("Structures" . 12))
            (xwidget-plus-follow-link-prepare-links links)))))

(ert-deftest test-xwidget-plus-follow-link-highlight ()
  (with-test-backend-browse '(0 0 1) 0 "links.html"
    (xwidget-plus-follow-link)
      (xwidget-plus-event-dispatch)
      (should (string= "test-1.html" (file-name-nondirectory (xwidget-webkit-current-url))))
      (should (equal (backend-test-link-classes backend "test-1") '["xwidget-plus-follow-link-selected"]))
      (should (equal (backend-test-link-classes backend "test-2") '["xwidget-plus-follow-link-candidate"])))
  (with-test-backend-browse '(1 0 1) 1 "links.html"
    (split-window-vertically)
    (save-excursion (switch-to-buffer-other-window "*Messages*"))
    (xwidget-plus-follow-link)
    (xwidget-plus-event-dispatch)
    (should (string= "test-2.html" (file-name-nondirectory (xwidget-webkit-current-url))))
    (should (equal (backend-test-link-classes backend "test-1") '["xwidget-plus-follow-link-candidate"]))
    (should (equal (backend-test-link-classes backend "test-2") '["xwidget-plus-follow-link-selected"]))))

(defmacro with-read-fixtures (backend &rest body)
  (declare (indent 1))
  `(let* ((links '(("test 1" . 0) ("test 2" . 1)))
          link
          (action (lambda (l) (setq link l)))
          (update (lambda ())))
     (with-backend ,backend
       (with-browse "links.html"
         ,@body))))

(ert-deftest test-xwidget-plus-follow-link-read-default ()
  (with-read-fixtures default
    (with-simulated-input "test SPC 2 RET"
      (xwidget-plus-follow-link-read backend "Test: " links action update)
      (should (= 1 link)))))

(ert-deftest test-xwidget-plus-follow-link-read-ido ()
  (with-read-fixtures ido
    (with-simulated-input "2 RET"
      (xwidget-plus-follow-link-read backend "Test: " links action update)
      (should (= 1 link)))))

(ert-deftest test-xwidget-plus-follow-link-read-ivy ()
  (with-read-fixtures ivy
    (with-simulated-input "2 RET"
      (xwidget-plus-follow-link-read backend "Test: " links action update)
      (should (= 1 link)))))

(ert-deftest test-xwidget-plus-follow-link-read-helm ()
  (with-read-fixtures helm
    (with-simulated-input '("2" (wsi-simulate-idle-time 0.1) "RET")
      (xwidget-plus-follow-link-read backend "Test: " links action update)
      (should (= 1 link)))))

;; Local Variables:
;; eval: (mmm-mode)
;; eval: (mmm-add-classes '((elisp-js :submode js-mode :face mmm-code-submode-face :delimiter-mode nil :front "--js \"" :back "\" js--")))
;; mmm-classes: elisp-js
;; End:

(provide 'xwidget-plus-follow-link-test)
;;; xwidget-plus-follow-link-test.el ends here
