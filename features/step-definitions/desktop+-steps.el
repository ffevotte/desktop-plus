;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I am in a fresh Emacs instance$"
       (lambda ()
         (desktop-kill)
         (setq desktop-dirname nil)
         (desktop-save-mode -1)
         (cd "~")
         (letf (((symbol-function 'yes-or-no-p)
                 (lambda (&args) t)))
           (mapc (lambda (buffer)
                   (when (not (memq buffer test/initial-buffers))
                     (kill-buffer buffer)))
                 (buffer-list)))
         (switch-to-buffer "*scratch*")))

(Given "^I switch to directory \"\\([^\"]+\\)\"$"
       (lambda (name)
         (cd name)))

(When "^I call M-x \"\\([^\"]+\\)\" RET \"\\([^\"]+\\)\" RET$"
      (lambda (command arg)
        (When "I start an action chain")
        (When "I press \"M-x\"")
        (When (format "I type \"%s\"" command))
        (When "I press \"RET\"")
        (When (format "I type \"%s\"" arg))
        (When "I execute the action chain")))

(When "I rename the buffer \"\\([^\"]+\\)\"$"
      (lambda (name)
        (rename-buffer name)))

(Then "^Desktop session \"\\([^\"]+\\)\" should exist$"
      (lambda (session)
        (let ((path (f-expand session desktop+-base-dir)))
          (cl-assert (f-exists? path) nil
                     "Expected path `%s' to exist"
                     path))))

(Then "^Buffer \"\\([^\"]+\\)\" should exist$"
      (lambda (name)
        (cl-assert (get-buffer name) nil
                   "Expected buffer `%s' to exist; list of current buffers:\n%s"
                   name (buffer-list))))

(Then "^Variable \"\\([^\"]+\\)\" should be \"\\([^\"]+\\)\"$"
      (lambda (var val)
        (cl-assert (string= val (symbol-value (intern var))) nil
                   "Expected `%s' to be `%s'; found `%s' instead"
                   var val (symbol-value (intern var)))))

(Then "^Program \"\\([^\"]+\\)\" should be running$"
      (lambda (program)
        (cl-assert (get-buffer-process (current-buffer)) nil
                   "No process running in the buffer")
        (let ((command (car (last (process-command
                                   (get-buffer-process (current-buffer)))))))
          (cl-assert (string= program command) nil
                     "Expected process `%s' to be running; found `%s' instead"
                     program command))))
