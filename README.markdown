# Wrap Region
Wrap Region is a minor mode for Emacs that wraps a region with
punctuations. For tagged markup modes, such as HTML and XML, it wraps
with tags.

## Installation

### Manually
Download **wrap-region.el** and put in Emacs load path. Then require it:
    $ git clone git://github.com/rejeep/wrap-region.git ~/.emacs.d
    
    (add-to-list 'load-path "~/.emacs.d/wrap-region")
    (require 'wrap-region)

## Usage
 To start wrap-region:
    (wrap-region-mode t) or M-x wrap-region-mode

If you only want wrap-region active in some mode, use a hook:
    (add-hook 'ruby-mode-hook 'turn-on-wrap-region-mode)

Or if you want to activate it in all buffers, use the global mode:
    (wrap-region-global-mode t)

Now select a region and hit one of the punctuation keys (**"**, **{**,
**(**, ...). If you are in a tag mode, try select a region and hit **<**.

## Hook it up
Wrap Region comes with these hooks:

* **wrap-region-hook**: Called when wrap-region-mode is started
* **wrap-region-before-insert-twice-hook**: Called before insert twice occurs
* **wrap-region-after-insert-twice-hook**: Called after insert twice occurs
* **wrap-region-before-wrap-hook**: Called before wrapping occurs
* **wrap-region-after-wrap-hook**: Called after wrapping occurs

### Use case
A useful way to use these hooks is for example in modes for languages
that use curly braces.
    (add-hook 'c-mode-hook 'wrap-region-indent-curly-braces)
    (defun wrap-region-indent-curly-braces ()
      (add-hook 'wrap-region-after-insert-twice-hook
                (lambda ()
                  (when (string= (char-to-string (char-before)) "{")
                    (let ((origin (line-beginning-position)))
                      (newline 2)
                      (indent-region origin (line-end-position))
                      (forward-line -1)
                      (indent-according-to-mode))))))

## Known problems

### Calc mode
In calc-mode you use ' (apostrophe) to enter an algebraic
expression. If you use the global wrap-region-mode, wrap-region will
be active when you start calc-mode and hence clash with the algebraic key.

You can solve this by adding calc-mode to the list of modes that
wrap-region should not be activated in:
    (setq wrap-region-except-modes '(calc-mode))
