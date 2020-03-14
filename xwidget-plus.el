;;; xwidget-plus.el --- Improve xwidget usability

;; Author: Damien Merenne
;; URL: https://github.com/canatella/xwidget-plus
;; Created: 2020-03-11
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "25.3"))

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

(require 'xwidget-plus-common)
(require 'xwidget-plus-follow-link)

;; Bring the window to front when summoning browse
(defun xwidget-plus-webkit-browse-url-advise (&rest _)
    "Advice to add switch to window when calling `xwidget-webkit-browse-url'."
    (switch-to-buffer-other-window (xwidget-buffer (xwidget-webkit-current-session))))
(advice-add #'xwidget-webkit-browse-url :after #'xwidget-plus-webkit-browse-url-advise)

;; Local Variables:
;; eval: (mmm-mode)
;; eval: (mmm-add-classes '((elisp-js :submode js-mode :face mmm-code-submode-face :delimiter-mode nil :front "--js \"" :back "\" js--")))
;; mmm-classes: elisp-js
;; End:

(provide 'xwidget-plus)
;;; xwidget-plus.el ends here
