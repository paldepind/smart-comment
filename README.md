# smart-comment

Smarter commenting for Emacs.

## About

Code gets rewritten when refactoring to accomidate for new features or
when fixing bugs. An approach used by many developers is to disable a
piece of code with comments, then rewrite an improved version below,
test that is works and then delete the commented code. smart-comment
makes this and similair workflows much swifter.

smart-comment is implemented on top of the commenting functions built
in to Emacs. It is meant to replace `comment-dwim` as the function you
bind to `M-;`.

It triggers different comment actions taking the current location of
the point into acount. Commenting out lines of code is faster.
Commented lines can be marked for later deletion and then all removed
with a single command.

## Demonstration

![Recording of smart-comment](/demo.gif?raw=true)

## Installation

Install from MELPA or put `smart-comment.el` somewhere in you load path.

```lisp
(add-to-list 'load-path "~/emacs.d")
(require 'smart-comment)
(global-set-key (kbd "M-;") 'smart-comment)
```

With use-package.

```lisp
(use-package smart-comment
  :bind ("M-;" . smart-comment))
```

## Tutorial

You should have `smart-comment` bound to `M-;` or another key of your
choosing. The default behaviour is described below.

Pressing `M-;` when the cursor is at the end of a line inserts a
comment at the end of the line. This is the same as the default
behaviour of `M-;`.

Pressing `M-;` when the cursor is not at the end of the line comments
out the line. If a universal argument is passed the line is also
marked for deletion. I.e. `C-u M-;` marks a line for deletion.

Passing two universal arguments, i.e. `C-u C-u M-;` deletes all lines
marked for deletion.

`smart-comment` tries to be smart about what it does. For instance
adding a mark to a line that is already marked will remove the mark
instead.

## `smart-comment`

`smart-comment` simply invokes other functions based on the cursors
current position on the line.

* At the beginning a line: Calls the function referred to by
  `smart-comment-beg-action`. By default this is `smart-comment-line`
  which will comment out the current line.
* In the middle of a line: Calls the function referred to by
  `smart-comment-mid-action`. By default this is `smart-comment-line`
  as above.
* The the end of a line: Calls the function referred to by
  `smart-comment-end-action`. By default this is `comment-dwim`—the
  function Emacs binds to `M-;` by default.
  
If the region is active `smart-comment` calls `smart-comment-region`.
When invoked with two universal arguments it calls
`smart-comment-cleanup` which deletes all lines marked for deletion.


## Thanks

Thanks to [@limemloh](https://github.com/limemloh/) for bug finding
and feedback.
