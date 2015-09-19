(require 'f)

(defvar desktop+-support-path
  (f-dirname load-file-name))

(defvar desktop+-features-path
  (f-parent desktop+-support-path))

(defvar desktop+-root-path
  (f-parent desktop+-features-path))

(defvar desktop+-sandbox-path
  (f-expand "sandbox/" desktop+-root-path))

(add-to-list 'load-path desktop+-root-path)

(require 'undercover)
(undercover "*.el")

(require 'desktop+)
(require 'espuds)
(require 'ert)
(require 'cl)

(Setup
 (setq desktop+-base-dir (concat desktop+-sandbox-path "/"))
 (when (f-exists? desktop+-sandbox-path)
   (f-delete desktop+-sandbox-path :force))
 (f-mkdir desktop+-sandbox-path)

 (add-to-list 'desktop-clear-preserve-buffers "desktop\\+.el")
 (find-file "desktop+.el")
 (setq test/initial-buffers (buffer-list)))

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
