;;; desktop+.el --- improved sessions

;; Copyright (C) 2014 François Févotte
;; Author:  François Févotte <fevotte@gmail.com>
;; URL:
;; Version: 0.1

;; This file is NOT part of Emacs

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

;;     `desktop+' extends `desktop' by providing more features related to
;;     sessions persistance.

;; Centralized directory storing all desktop sessions:
;;
;;     instead of relying on Emacs' starting directory to choose the session
;;     Emacs restarts, two functions are provided to manipulate sessions by
;;     name.
;;
;;     `desktop-create': create a new session and give it a name.
;;
;;     `desktop-load': change the current session; the new session to be loaded
;;          is identified by its name, as given during session creation using
;;          `desktop-create'.
;;
;;     The currently active session is identified in the title bar. You can
;;     customize `desktop-frame-title-function' to change the way the active
;;     session is displayed.
;;
;;     All sessions managed this way are stored in the directory given by
;;     `desktop-base-dir'.

;;; Code:
(eval-when-compile
  (require 'desktop))

(defvar desktop-base-dir "~/.emacs.d/desktops/"
  "Base directory for desktop files")

;;;###autoload
(defun desktop-create ()
  "Create a new session, identified by a name.
The session is created in a subdirectory of
`desktop-base-dir'. It can afterwards be reloaded using
`desktop-load'."
  (interactive)
  (when (or (not (boundp 'desktop-dirname))
            (null desktop-dirname))
    (let ((name (read-from-minibuffer "Desktop name: ")))
      (setq desktop-dirname (concat desktop-base-dir name))
      (make-directory desktop-dirname 'parents)))
  (desktop-save desktop-dirname)
  (desktop--set-frame-title)
  (desktop-save-mode 1))

;;;###autoload
(defun desktop-load (name)
  "Load a session previously created using `desktop-create'."
  (interactive
   (list
    (completing-read "Desktop name: "
                     (remove "." (remove ".." (directory-files desktop-base-dir))))))
  (desktop-change-dir (concat desktop-base-dir name))
  (desktop--set-frame-title)
  (desktop-save-mode 1))


(defun desktop--frame-title (desktop-name)
  "Default frame title function for sessions.

Returns the following frame title format:
  '%b - Emacs [DESKTOP-NAME]'"
  (list (concat "%b - Emacs [" desktop-name "]")))

(defvar desktop-frame-title-function 'desktop--frame-title
  "Function returning the frame title when a desktop session is loaded.

This function must accept the desktop name as a string argument
and return a frame title format suitable for setting
`frame-title-format'")

(defun desktop--set-frame-title ()
  "Set the frame title to show the currently active session."
  (setq frame-title-format
        (funcall desktop-frame-title-function
                 (file-name-nondirectory (directory-file-name desktop-dirname)))))

(provide 'desktop+)

;;; desktop+.el ends here