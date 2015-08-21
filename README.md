# desktop+

`desktop+` extends `desktop` by providing more features related to sessions persistance.


1. Instead of relying on Emacs' starting directory to choose the session Emacs restarts, sessions are manipulated by name. All information about them is stored into a centralized directory.

2. Desktop sessions by default save only buffers associated to "real" files. Desktop+ extends this by handling also "special buffers", such as those in `compilation-mode` or `term-mode`.



## Installation and setup

From `git`:

1. get the repository:

   ```sh
   $ git clone https://github.com/ffevotte/desktop-plus.git
   ```

2. add the following snippet to your init file:

   ```lisp
   (add-to-list 'load-path "/path/to/desktop-plus")
   (require 'desktop+)
   ```

3. optionally register supported major modes for special handling:

    ```lisp
    (desktop+/special-buffer-handlers)
    ```

    or only some of them:

    ```lisp
    (add-to-list 'desktop+/special-buffer-handlers 'term-mode)
    (add-to-list 'desktop+/special-buffer-handlers 'compilation-mode)
    ```

## Usage

Two functions are defined to manipulate desktop sessions by name:

- `desktop-create`: create a new session and give it a name.

- `desktop-load`: change the current session; the new session to be loaded is identified by its name, as given during session creation using `desktop-create`. The currently active session is identified in the title bar.

Once created or loaded, sessions are automatically saved when exiting emacs or changing session.


## Customization

- `desktop-base-dir`: directory where all information about desktop sessions are stored. The default value is `"~/.emacs.d/desktops/"`.

- `desktop-frame-title-function`: function called to get the new frame title format when the session changes. This function must take the desktop session name as a string argument and return a frame title format suitable for setting `frame-title-format`.

  Customize it to change the way session names are displayed. For example:

    ```lisp
    (defun my/desktop-frame-title-function (desktop-name)
      (list (concat "%b - Emacs [" desktop-name "]")))

    (setq desktop-frame-title-function
          'my/desktop-frame-title-function)
    ```

- `desktop+/special-buffer-modes`: list of major modes for which buffers should be handled specially.


## API

- `(desktop+/add-handler NAME PRED SAVE-FN LOAD-FN &optional ACTIVATE)`

    Add handlers for special buffers.

    NAME is a symbol identifying the handler for later activation or deactivation.

    PRED should be a unary function used as a predicate to determine whether a buffer should be handled specially. When called in a buffer which should be handled, PRED should return non-nil.  As a special case, if PRED is nil, NAME is interpreted as a major mode name for which to test.

    SAVE-FN should be a function taking no parameter, returning a list of all relevant parameters for the current buffer, which is assumed to be in the given major mode.

    LOAD-FN should be a function of the form `(lambda (name &rest args) ...)` allowing to restore a buffer named NAME in major mode MODE, from information stored in ARGS, as determined by SAVE-FN.

    If ACTIVATE is non-nil, also add MODE to the list of handled modes in `desktop+/special-buffer-handlers`.
