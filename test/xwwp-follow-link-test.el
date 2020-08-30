;;; xwwp-follow-link-test.el -- xwwp follow link test suite -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'with-simulated-input)
(require 'xwwp-follow-link)
(require 'xwwp-follow-link-ido)
(require 'xwwp-follow-link-ivy)
(require 'xwwp-follow-link-helm)

(setq completing-read-function #'completing-read-default)

;; Some usefull javascript
(xwwp-js-def test element-classes (selector)
  "Fetch the list of css class for element matching SELECTOR.""
map = Array.prototype.map;
r = {};
document.querySelectorAll(selector).forEach(l => {
    r[l.id] = map.call(l.classList, t => {
        return t.toString();
     });
});
return r;
")

(xwwp-js-def test current-location ()
  "Fetch the current url.""
return '' + window.location;
")

;; A mocked backend class that doesn't interactively read anything.
(defclass xwwp-follow-link-completion-backend-test (xwwp-follow-link-completion-backend)
  ((candidates-mock :initarg :candidates-mock)
   (selected-mock :initarg :selected-mock)
   (action-fn)
   (classes)
   (location)))

(defun xwwp-update-fn-callback (result)
  "Called after updating candidates with the css classes in RESULT."
  (let ((backend xwwp-follow-link-completion-backend-instance))
    ;; Store the results.
    (oset backend classes (seq-map #'identity result))
    ;; Trigger the action function with the mocked selected link
    (funcall (oref backend action-fn) (oref backend selected-mock))
    (xwwp-event-dispatch)))

(defun xwwp-location-callback (result)
  "Called after updating candidates with the css classes in RESULT."
  (let ((backend xwwp-follow-link-completion-backend-instance))
    ;; Store the results.
    (oset backend location result)))

(cl-defmethod xwwp-follow-link-candidates ((backend xwwp-follow-link-completion-backend-test))
  "Return the list of BACKEND mocked candidates."
  (oref backend candidates-mock))

(cl-defmethod xwwp-follow-link-read ((backend xwwp-follow-link-completion-backend-test) _ _ action-fn update-fn)
  "Store ACTION-FN in BACKEND, call the UPDATE-FN, and fetch the link element classes."
  ;; Stoire action so that we can call it after having fetch the css classes.
  (oset backend action-fn action-fn)
  ;; Trigger the javascript update.
  (funcall update-fn)
  ;; Fetch css classes.
  (xwwp-js-inject (xwidget-webkit-current-session) 'test)
  (xwwp-test-element-classes (xwidget-webkit-current-session) "a" #'xwwp-update-fn-callback)
  (xwwp-event-dispatch))

(cl-defmethod backend-test-link-classes ((backend xwwp-follow-link-completion-backend-test) link-id)
  "Return test BACKEND css class names for LINK-ID."
  (cdr (assoc link-id (oref backend classes))))

(defmacro with-backend (backend &rest body)
  "Run BODY with the specified BACKEND."
  (declare (indent 1))
  `(let* ((backend (,(intern (concat "xwwp-follow-link-completion-backend-" (symbol-name backend)))))
          (xwwp-follow-link-completion-backend-instance backend))
     ,@body))


(defmacro with-test-backend-browse (candidates selected url &rest body)
  "Run BODY with the specified BACKEND mocking CANDIDATES and SELECTED while browsing URL."
  (declare (indent 3))
  `(let* ((xwwp-follow-link-completion-system
           (lambda () (xwwp-follow-link-completion-backend-test :candidates-mock ,candidates
                                                                :selected-mock ,selected))))
     (with-browse ,url
       ,@body)))

(ert-deftest test-xwwp-follow-link-prepare-links ()
  (let ((links '(("3" . ["Functions" "http://www.this.is.a.test.de/functions"])
                 ("1" . ["Function Cells" "http://www.this.is.a.test.de/function-cells"])
                 ("12" . ["Structures" "http://www.this.is.a.test.de/structures"])
                 ("2" . ["Anonymous Functions" "http://www.this.is.a.test.de/anon"])
                 ("9" . ["Declare Form" "http://www.this.is.a.test.de/declare-form"]))))
    (should (equal '(("Function Cells" . (1 "http://www.this.is.a.test.de/function-cells"))
	             ("Anonymous Functions" . (2 "http://www.this.is.a.test.de/anon"))
	             ("Functions" . (3 "http://www.this.is.a.test.de/functions"))
	             ("Declare Form" . (9 "http://www.this.is.a.test.de/declare-form"))
	             ("Structures" . (12 "http://www.this.is.a.test.de/structures")))
            (xwwp-follow-link-prepare-links links)))))

(ert-deftest test-xwwp-follow-link-highlight ()
  (with-test-backend-browse '((0 "http://") (0 "http://") (1 "http://")) '(0 "http://") "links.html"
    (xwwp-follow-link)
    (xwwp-event-loop)
    (let ((backend xwwp-follow-link-completion-backend-instance))
      (xwwp-js-inject xwidget 'test)
      (xwwp-test-current-location xwidget #'xwwp-location-callback)
      (xwwp-event-dispatch)
      (should (string= "test-1.html" (file-name-nondirectory (oref backend location))))
      (should (equal (backend-test-link-classes backend "test-1") '["xwwp-follow-link-selected"]))
      (should (equal (backend-test-link-classes backend "test-2") '["xwwp-follow-link-candidate"]))))
  (with-test-backend-browse '((1 "http://") (0 "http://") (1 "http://")) '(1 "http://") "links.html"
    (xwwp-follow-link)
    (xwwp-event-loop)
    (let ((backend xwwp-follow-link-completion-backend-instance))
      (xwwp-js-inject xwidget 'test)
      (xwwp-test-current-location xwidget #'xwwp-location-callback)
      (xwwp-event-dispatch)
      (should (string= "test-2.html" (file-name-nondirectory (oref backend location))))
      (should (equal (backend-test-link-classes backend "test-1") '["xwwp-follow-link-candidate"]))
      (should (equal (backend-test-link-classes backend "test-2") '["xwwp-follow-link-selected"]))))
  (with-test-backend-browse '((1 "http://")) '(1 "http://") "links.html"
    (xwwp-follow-link)
    (xwwp-event-loop)
    (let ((backend xwwp-follow-link-completion-backend-instance))
      (xwwp-js-inject xwidget 'test)
      (xwwp-test-current-location xwidget #'xwwp-location-callback)
      (xwwp-event-dispatch)
      (should (string= "test-2.html" (file-name-nondirectory (oref backend location))))
      (should (equal (backend-test-link-classes backend "test-1") '[]))
      (should (equal (backend-test-link-classes backend "test-2") '["xwwp-follow-link-selected"])))))

(defmacro with-read-fixtures (backend &rest body)
  (declare (indent 1))
  `(let* ((links '(("test 1" . 0) ("test 2" . 1)))
          link
          (action (lambda (l) (setq link l)))
          (update (lambda ())))
     (with-backend ,backend
       (with-browse "links.html"
         ,@body))))

(ert-deftest test-xwwp-follow-link-read-default ()
  (with-read-fixtures default
    (with-simulated-input "test SPC 2 RET"
      (xwwp-follow-link-read backend "Test: " links action update)
      (should (= 1 link)))))

(ert-deftest test-xwwp-follow-link-read-ido ()
  (require 'ido)
  (with-read-fixtures ido
    (with-simulated-input "2 RET"
      (xwwp-follow-link-read backend "Test: " links action update)
      (should (= 1 link)))))

(ert-deftest test-xwwp-follow-link-read-ivy ()
  (require 'ivy)
  (with-read-fixtures ivy
    (with-simulated-input "2 RET"
      (xwwp-follow-link-read backend "Test: " links action update)
      (should (= 1 link)))))

(ert-deftest test-xwwp-follow-link-read-helm ()
  (require 'helm)
  (with-read-fixtures helm
    (with-simulated-input '("2" (wsi-simulate-idle-time 0.1) "RET")
      (xwwp-follow-link-read backend "Test: " links action update)
      (should (= 1 link)))))

(defmacro with-feature (feature &rest body)
  (declare (indent 1))
  (let ((fsym (intern (concat "xwwp-follow-link-" (symbol-name feature)))))
    `(cl-letf (((symbol-function 'require) (lambda (f &optional filename no-errors) (eq (quote ,fsym) f))))
       ,@body)))

(ert-deftest test-xwwp-follow-link-make-backend-use-custom ()
  (let ((xwwp-follow-link-completion-system 'default))
    (with-feature nil
      (should (eq #'xwwp-follow-link-completion-backend-default (xwwp-follow-link-make-backend)))))
  (let ((xwwp-follow-link-completion-system 'ido))
    (with-feature ido
      (should (eq #'xwwp-follow-link-completion-backend-ido (xwwp-follow-link-make-backend)))))
  (let ((xwwp-follow-link-completion-system 'ivy))
    (with-feature ivy
      (should (eq #'xwwp-follow-link-completion-backend-ivy (xwwp-follow-link-make-backend)))))
  (let ((xwwp-follow-link-completion-system 'helm))
    (with-feature helm
      (should (eq #'xwwp-follow-link-completion-backend-helm (xwwp-follow-link-make-backend)))))
  (let ((xwwp-follow-link-completion-system #'identity))
    (should (eq #'identity (xwwp-follow-link-make-backend)))))

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

(provide 'xwwp-follow-link-test)
;;; xwwp-follow-link-test.el ends here
