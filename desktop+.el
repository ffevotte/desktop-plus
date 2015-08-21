;;; desktop+.el --- improved sessions

;; Copyright (C) 2014 François Févotte
;; Author:  François Févotte <fevotte@gmail.com>
;; URL: https://github.com/ffevotte/desktop-plus
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
;;     Instead of relying on Emacs' starting directory to choose the session
;;     Emacs restarts, two functions are provided to manipulate sessions by
;;     name.
;;
;;     `desktop-create': create a new session and give it a name.
;;
;;     `desktop-load': change the current session; the new session to be loaded
;;          is identified by its name, as given during session creation using
;;          `desktop-create'.
;;
;;     The currently active session is identified in the title bar.  You can
;;     customize `desktop-frame-title-function' to change the way the active
;;     session is displayed.
;;
;;     All sessions managed this way are stored in the directory given by
;;     `desktop-base-dir'.

;; Handling of special buffers:
;;
;;     Desktop sessions by default save only buffers associated to "real" files.
;;     Desktop+ extends this by handling also "special buffers", such as those
;;     in `compilation-mode' or `term-mode'.

;;; Code:
(eval-when-compile
  (require 'desktop)
  (require 'dash))

;; * Named sessions

;; ** Customizable options

(defvar desktop-base-dir "~/.emacs.d/desktops/"
  "Base directory for desktop files.")

(defvar desktop-frame-title-function 'desktop+--frame-title
  "Function returning the frame title when a desktop session is loaded.

This function must accept the desktop name as a string argument
and return a frame title format suitable for setting
`frame-title-format'")

;; ** Entry points

;;;###autoload
(defun desktop-create (name)
  "Create a new session, identified by a name.
The session is created in a subdirectory of
`desktop-base-dir'.  It can afterwards be reloaded using
`desktop-load'."
  (interactive "MDesktop name: ")
  (desktop-kill)
  (setq desktop-dirname (concat desktop-base-dir name))
  (make-directory desktop-dirname 'parents)
  (desktop-save desktop-dirname)
  (desktop+--set-frame-title)
  (desktop-save-mode 1))

;;;###autoload
(defun desktop-load (name)
  "Load a session previously created using `desktop-create'.
NAME is the name which was given at session creation.  When called
interactively, it is asked in the minibuffer with
auto-completion."
  (interactive
   (list
    (completing-read "Desktop name: "
                     (remove "." (remove ".." (directory-files desktop-base-dir))))))
  (desktop-change-dir (concat desktop-base-dir name))
  (desktop+--set-frame-title)
  (desktop-save-mode 1))

;; ** Inner workings

(defun desktop+--frame-title (desktop-name)
  "Default frame title function for sessions.

Returns the following frame title format:
  '%b - Emacs [DESKTOP-NAME]'"
  (list (concat "%b - Emacs [" desktop-name "]")))


(defun desktop+--set-frame-title ()
  "Set the frame title to show the currently active session."
  (setq frame-title-format
        (funcall desktop-frame-title-function
                 (file-name-nondirectory (directory-file-name desktop-dirname)))))


;; * Special buffers

;; ** Customizable options

(defvar desktop+/special-buffer-handlers nil
  "List of major modes to be handled.")

;; ** Entry point

;;;###autoload
(defun desktop-save--desktop+ (&rest args)
  "Also save special buffers."
  (desktop+--buffers-save))
(advice-add 'desktop-save :before #'desktop-save--desktop+)

;;;###autoload
(defun desktop-restore-frameset--desktop+ (&rest args)
  (desktop+--buffers-load))
(advice-add 'desktop-restore-frameset :before #'desktop-restore-frameset--desktop+)

;;;###autoload
(defun desktop+/special-buffer-handlers ()
  (add-to-list 'desktop+/special-buffer-handlers 'term-mode)
  (add-to-list 'desktop+/special-buffer-handlers 'compilation-mode)
  (add-to-list 'desktop+/special-buffer-handlers 'indirect-buffer))

;; ** Mode-specific handlers for special buffers

(defvar desktop+--special-buffer-handlers nil
  "Alist of handlers for special buffers.")

(defun desktop+/add-handler (name pred save-fn load-fn &optional activate)
  "Add handlers for special buffers.

NAME is a symbol identifying the handler for later activation or
deactivation.

PRED should be a unary function used as a predicate to determine
whether a buffer should be handled specially. When called in a
buffer which should be handled, PRED should return non-nil.
As a special case, if PRED is nil, NAME is interpreted as a major
mode name for which to test.

SAVE-FN should be a function taking no parameter, returning a
list of all relevant parameters for the current buffer, which is
assumed to be in the given major mode.

LOAD-FN should be a function of the following form:

  (lambda (name &rest args) ...)

allowing to restore a buffer named NAME in major mode MODE,
from information stored in ARGS, as determined by SAVE-FN.

If ACTIVATE is non-nil, also add MODE to the list of handled
modes in `desktop+/special-buffer-handlers'."
  (declare (indent 1))
  (when (null pred)
    (setq pred (eval `(lambda () (eq major-mode ',name)))))
  (when activate
    (add-to-list 'desktop+/special-buffer-handlers name))
  (push (list name pred save-fn load-fn)
        desktop+--special-buffer-handlers))

;; *** Terminals

(desktop+/add-handler 'term-mode
  nil

  (lambda ()
    "Return relevant parameters for saving a terminal buffer."
    (list :dir     default-directory
          :command (car (last (process-command
                               (get-buffer-process (current-buffer)))))))

  (lambda (name &rest args)
    "Restore a terminal buffer from saved parameters."
    (when (null (get-buffer name))
      (let ((default-directory (plist-get args :dir)))
        (with-current-buffer (term (plist-get args :command))
          (rename-buffer name))))))

;; *** Compilation buffers

(eval-when-compile
  (require 'compile))

(desktop+/add-handler 'compilation-mode
  nil

  (lambda ()
    "Return relevant parameters for saving a compilation buffer."
    (list :command `(quote ,compilation-arguments)
          :dir     compilation-directory))

  (lambda (name &rest args)
    "Restore a compilation buffer from saved parameters."
    (with-current-buffer (get-buffer-create name)
      (compilation-mode)
      (set (make-local-variable 'compilation-arguments) (plist-get args :command))
      (set (make-local-variable 'compilation-directory) (plist-get args :dir)))))

;; *** Clones (indirect buffers)

(desktop+/add-handler 'indirect-buffer
  #'buffer-base-buffer

  (lambda ()
    `(:base ,(buffer-name (buffer-base-buffer))))

  (lambda (name &rest args)
    (with-current-buffer (get-buffer (plist-get args :base))
      (clone-indirect-buffer name nil))))

;; ** Inner workings

(defun desktop+--buffers-file ()
  "Name of the file where special buffers configuration will be saved."
  (concat desktop-dirname "/.emacs-buffers"))

(defun desktop+--create-buffer (key name &rest args)
  "Recreate a special buffer from saved parameters.

 should be registered in `desktop+--special-buffer-handlers'.

NAME is the name of the buffer.

ARGS is the relevant buffer parameters, as determined by the
registered save handler.  These parameters will be restored by
calling the load handler."
  (let ((handler (assq key desktop+--special-buffer-handlers)))
    (when handler
      (apply (nth 3 handler) name args))))

(defun desktop+--buffers-save ()
  "Persistently save special buffers.
Information is kept in the file pointed to by `desktop+--buffers-file'."
  (with-temp-buffer
    (mapc (lambda (b)
            (let ((data
                   (with-current-buffer b
                     (let ((handler
                            (--first
                             (and (memq (nth 0 it) desktop+/special-buffer-handlers)
                                  (funcall (nth 1 it)))
                             desktop+--special-buffer-handlers)))
                       (when handler
                         (append `(desktop+--create-buffer
                                   (quote ,(nth 0 handler))
                                   ,(buffer-name))
                                 (funcall (nth 2 handler))))))))
              (if data
                  (pp data (current-buffer)))))
          (buffer-list))
    (write-region nil nil (desktop+--buffers-file))))

(defun desktop+--buffers-load ()
  "Load special buffers from the persistent session file.
Information is kept in the file pointed to by
`desktop+--buffers-file'."
  (when (file-exists-p (desktop+--buffers-file))
    (load-file (desktop+--buffers-file))))

(provide 'desktop+)

;;; desktop+.el ends here
