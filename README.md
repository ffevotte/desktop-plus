# desktop+

`desktop+` extends `desktop` by providing more features related to sessions persistance.

Instead of relying on Emacs' starting directory to choose the session Emacs restarts, sessions are manipulated by name. All information about them is stored into a centralized directory.


## Installation

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
