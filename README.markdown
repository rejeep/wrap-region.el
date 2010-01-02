# Wrap Region

wrap-region is a minor mode that wraps text with punctuations. For
some tagged markup modes, such as HTML and XML, it wraps a region
with a tag instead of a punctuation.

## Installation
Add wrap-region to Emacs load-path
    (add-to-list 'load-path "/path/to/directory/or/file")

Then require wrap-region
    (require 'wrap-region)

To start wrap-region
    (wrap-region-mode t) or M-x wrap-region-mode

Or if you want it to be done automatically
    (add-hook 'find-file-hook 'wrap-region-mode)
