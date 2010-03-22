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
    (add-hook 'ruby-mode-hook 'wrap-region)

Or if you want to activate it in all buffers, use the global mode:
    (wrap-region-global-mode t)

Now select a region and hit one of the punctuation keys (**"**, **{**,
**(**, ...). If you are in a tag mode, try select a region and hit **<**.
