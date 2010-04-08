# Wrap Region
Wrap Region is a minor mode for Emacs that wraps a region with
punctuations. For tagged markup modes, such as HTML and XML, it wraps
with tags.

## Installation
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
    (add-hook 'wrap-region-after-insert-twice-hook
              (lambda ()
                (let ((modes '(c-mode java-mode javascript-mode css-mode)))
                  (if (and (string= (char-to-string (char-before)) "{") (member major-mode modes))
                      (let ((origin (line-beginning-position)))
                        (newline 2)
                        (indent-region origin (line-end-position))
                        (forward-line -1)
                        (indent-according-to-mode))))))


## Gotchas

### Except modes
In some modes, such as calc and dired, you do not want to have
wrap-region active because the key bindings will conflict. wrap-region
stores a list of modes (wrap-region-except-modes) in which wrap-region
will be inactive.

Some modes are added to the except list by default. See the list with:
    (describe-variable 'wrap-region-except-modes)
    
You can also add new modes with:
    (add-to-list 'wrap-region-except-modes 'conflicting-mode)
