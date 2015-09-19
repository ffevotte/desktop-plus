# desktop+
[![Build-Status](https://travis-ci.org/ffevotte/desktop-plus.svg)](https://travis-ci.org/ffevotte/desktop-plus) [![Coverage-Status](https://coveralls.io/repos/ffevotte/desktop-plus/badge.svg?branch=master&service=github)](https://coveralls.io/github/ffevotte/desktop-plus?branch=master) [![Tag](https://img.shields.io/github/tag/ffevotte/desktop-plus.svg)](https://github.com/ffevotte/desktop-plus/releases) [![MELPA](http://melpa.org/packages/desktop+-badge.svg)](http://melpa.org/#/desktop%2B) [![License](https://img.shields.io/badge/license-GPL_v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

`desktop+` extends `desktop` by providing more features related to sessions persistance.


1. Instead of relying on Emacs' starting directory to choose the session Emacs restarts, sessions are manipulated by name. All information about them is stored into a centralized directory.

2. Desktop sessions by default save only buffers associated to "real" files. Desktop+ extends this by handling also "special buffers", such as those in `compilation-mode` or `term-mode`.



## Installation

`desktop+` can be found on MELPA and it's the recommended way of installing it.

Otherwise, you can install it manually. First install `desktop+` dependencies:
[`dash`](http://github.com/magnars/dash.el) and
[`f`](http://github.com/rejeep/f.el). Then:

1. clone the git repository:

   ```sh
   $ git clone https://github.com/ffevotte/desktop-plus.git
   ```

2. tell emacs where to find it, for example by adding a snippet like this in
   your `init.el` file:

   ```lisp
   (add-to-list 'load-path "/path/to/desktop-plus")
   (require 'desktop+)
   ```


## Usage

### From Emacs

#### Named sessions

Two functions are defined to manipulate desktop sessions by name:

- `desktop-create`: create a new session and give it a name.

- `desktop-load`: change the current session; the new session to be loaded is identified by its name, as given during session creation using `desktop-create`. The currently active session is identified in the title bar.

As a special case, if the session name is left blank when calling one of these two functions, a name is automatically derived from the current working directory (see "Auto-named sessions" below).

Once created or loaded, sessions are automatically saved when exiting emacs or changing session.

#### Auto-named sessions

It is also possible to create and load sessions without explicitly specifying a name. These sessions are then automatically named after the current working directory. This can be done either by leaving the session name blank when calling `desktop-create` or `desktop-load`, or by using their dedicated counterparts:

- `desktop-create-auto`: create a new auto-named session.

- `desktop-load-auto`: load a previously created auto-named session.


### From the shell command-line

If you want to invoke Emacs from the command-line and specify a session to load at startup, you can define the following useful bash function:

```sh
function emacs-desktop () {
  emacs --eval '(desktop-load "'"$1"'")'
}
```

You can then invoke a named session directly from the command-line:

```sh
$ emacs-desktop my-session
```

or an auto-named session:

```sh
$ cd /path/to/working/directory
$ emacs-desktop
```

## Customization

- `desktop+-base-dir`: directory where all information about desktop sessions are stored. The default value is `"~/.emacs.d/desktops/"`.

- `desktop+-frame-title-function`: function called to get the new frame title format when the session changes. This function must take the desktop session name as a string argument and return a frame title format suitable for setting `frame-title-format`.

  Customize it to change the way session names are displayed. For example:

    ```lisp
    (defun my/desktop-frame-title-function (desktop-name)
      (list (concat "%b - Emacs [" desktop-name "]")))

    (setq desktop+-frame-title-function
          'my/desktop-frame-title-function)
    ```

- `desktop+-special-buffer-handlers`: list of special buffer types which should
  be handled specially. The default value contains all known types. You can
  remove some of them if you want.

    ```lisp
    ;; remove items from the list if you don't want a specific special buffer
    ;; type to be handled
    (setq desktop+-special-buffer-handlers
          '(term-mode
            compilation-mode
            indirect-buffer))
    ```


## API

- `(desktop+-add-handler NAME PRED SAVE-FN LOAD-FN &optional ACTIVATE)`

    Add handlers for special buffers.

    NAME is a symbol identifying the handler for later activation or deactivation.

    PRED should be a unary function used as a predicate to determine whether a buffer should be handled specially. When called in a buffer which should be handled, PRED should return non-nil.  As a special case, if PRED is nil, NAME is interpreted as a major mode name for which to test.

    SAVE-FN should be a function taking no parameter, returning a list of all relevant parameters for the current buffer, which is assumed to be in the given major mode.

    LOAD-FN should be a function of the form `(lambda (name &rest args) ...)` allowing to restore a buffer named NAME in major mode MODE, from information stored in ARGS, as determined by SAVE-FN.

    If ACTIVATE is non-nil, also add MODE to the list of handled modes in `desktop+-special-buffer-handlers`.


## Contributing

If you make improvements to this code or have suggestions, please do not
hesitate to fork the repository or submit bug reports on
[github](https://github.com/ffevotte/desktop-plus). The repository's URL is:

    https://github.com/ffevotte/desktop-plus.git


## License

Copyright (C) 2014-2015 François Févotte.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU
General Public License as published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.  If not,
see <http://www.gnu.org/licenses/>.
