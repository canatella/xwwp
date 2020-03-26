;;; xwwp-test.el -- xwwp test suite -*- lexical-binding: t; -*-

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

;; Test suite for xwidget-webkit-plus feature

;;; Code:

(require 'test-helper)
(require 'xwwp)

(ert-deftest test-xwwp-js-register-function ()
  (let ((xwwp-js-scripts '()))
    (xwwp-js-register-function 'namespace1 'fun1 "script 1.0")
    (should (equal '((namespace1  . ((fun1 . "script 1.0")))) xwwp-js-scripts))
    (xwwp-js-register-function 'namespace1 'fun1 "script 1.1")
    (should (equal '((namespace1  . ((fun1 . "script 1.1")))) xwwp-js-scripts))
    (xwwp-js-register-function 'namespace1 'fun2 "script 2.0")
    (should (equal '((namespace1  . ((fun2 . "script 2.0") (fun1 . "script 1.1")))) xwwp-js-scripts))
    (xwwp-js-register-function 'namespace1 'fun2 "script 2.1")
    (should (equal '((namespace1  . ((fun2 . "script 2.1") (fun1 . "script 1.1")))) xwwp-js-scripts))
    (xwwp-js-register-function 'namespace1 'fun1 "script 1.2")
    (should (equal '((namespace1  . ((fun1 . "script 1.2") (fun2 . "script 2.1")))) xwwp-js-scripts))
    (xwwp-js-register-function 'namespace2 'fun1 "script 1.0")
    (should (equal '((namespace2  . ((fun1 . "script 1.0")))
                     (namespace1  . ((fun1 . "script 1.2") (fun2 . "script 2.1"))))
                   xwwp-js-scripts))
    (xwwp-js-register-function 'namespace2 'fun2 "script 2.0")
    (should (equal '((namespace2  . ((fun2 .  "script 2.0") (fun1 . "script 1.0")))
                     (namespace1  . ((fun1 . "script 1.2") (fun2 . "script 2.1"))))
                   xwwp-js-scripts))))

(provide 'xwwp-test)
;;; xwwp-test.el ends here
