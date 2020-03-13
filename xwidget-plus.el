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
(require 'ivy)
(require 'eieio)

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


(provide 'xwidget-link)

;;; xwidget-link.el ends here

;; Local Variables:
;; eval: (mmm-mode)
;; eval: (mmm-add-classes '((elisp-js :submode js-mode :face mmm-code-submode-face :delimiter-mode nil :front "--js \"" :back "\" js--")))
;; mmm-classes: elisp-js
;; End:
