# Wrap Region

Wrap Region is a minor mode for Emacs that wraps a region with
punctuations. For "tagged" markup modes, such as HTML and XML, it
wraps with tags.

See comments in `wrap-region.el` for installation and usage information.


## Hook it up
Wrap Region comes with these hooks:

* `wrap-region-hook` Called when mode is started
* `wrap-region-before-wrap-hook` Called before wrap
* `wrap-region-after-wrap-hook` Called after wrap


## Gotchas

### Except modes

In some modes, such as `calc-mode` and `dired-mode`, you don't want to
have wrap-region active because the key bindings will
conflict. wrap-region stores a list of modes (see
`wrap-region-except-modes`) in which wrap-region will be inactive.

Some modes are added to the except list by default. See the list with:

    (describe-variable 'wrap-region-except-modes)
    
To add a mode, do this:

    (add-to-list 'wrap-region-except-modes 'conflicting-mode)
