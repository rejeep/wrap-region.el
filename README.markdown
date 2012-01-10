# Wrap Region
Wrap Region is a minor mode for Emacs that wraps a region with
punctuations. For "tagged" markup modes, such as HTML and XML, it
wraps with tags.

## Installation
I recommend installing via ELPA, but manual installation is simple as well:

    (add-to-list 'load-path "/path/to/wrap-region")
    (require 'wrap-region)
    
## Usage
Start `wrap-region-mode` using.

    (wrap-region-mode t)
    
or

    M-x wrap-region-mode

Now try selecting a region and press any of the following keys: `"`, `'`, `(`, `{`, `[`.

For more information, see comments in `wrap-region.el`.

## Hooks
Wrap Region has a few hooks hooks:

* `wrap-region-hook` Called when `wrap-region-mode` is started
* `wrap-region-before-wrap-hook` Called before a wrap occurs
* `wrap-region-after-wrap-hook` Called after a wrap occurs

## Gotchas

### Except modes
In some modes, such as `calc-mode` and `dired-mode`, you don't want to
have wrap region active since the key bindings will
conflict. Wrap region stores a list of modes (see
`wrap-region-except-modes`) in which wrap region will be inactive.

Some modes are added to the except list by default. See the list with:

    (describe-variable 'wrap-region-except-modes)
    
To add a new mode, do this:

    (add-to-list 'wrap-region-except-modes 'conflicting-mode)

## Contribution
Contribution is much welcome! Wrap region is tested using [Ecukes](http://ecukes.info). When
Adding New features, please write tests for them!

To fetch Ecukes:

    $ cd /path/to/wrap-region
    $ git submodule init
    $ git submodule update
    
Run the tests with:

    $ ./util/ecukes/ecukes features
