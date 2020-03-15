;;; xwidget-plus.el --- Improve xwidget usability -*- lexical-binding: t; -*-

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

;;;###autoload
(defun xwidget-plus-browse-url (url &optional new-session)
  "Ask xwidget-webkit to browse URL.
NEW-SESSION specifies whether to create a new xwidget-webkit session.
Interactively, URL defaults to the string looking like a url around point."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: "
                                             ;;(xwidget-webkit-current-url)
                                             )))
  (or (featurep 'xwidget-internal)
      (user-error "Your Emacs was not compiled with xwidgets support"))
  (when (stringp url)
    (if new-session
        (xwidget-webkit-new-session url)
      (progn (xwidget-webkit-goto-url url)
             (switch-to-buffer-other-window (xwidget-buffer (xwidget-webkit-current-session)))))))

(provide 'xwidget-plus)
;;; xwidget-plus.el ends here
