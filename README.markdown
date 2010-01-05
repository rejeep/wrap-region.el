# Wrap Region

wrap-region is a minor mode that wraps text with punctuations. For
some tagged markup modes, such as HTML and XML, it wraps a region
with a tag instead of a punctuation.

## Installation

### Manually
First download the **wrap-region.el** file. If you use git you can
fetch it from Github
    $ git://github.com/rejeep/wrap-region.git ~/.emacs.d/packages
    
Make sure it's in Emacs load-path
    (add-to-list 'load-path "~/.emacs.d/packages/wrap-region")
    
Then require it
    (require 'wrap-region)
    
### ELPA
wrap-region is in ELPA. If you use ELPA, I recommend you to fetch it
from there.

## Usage
To start wrap-region
    (wrap-region-mode t) or M-x wrap-region-mode

Or if you want it to be done automatically
    (add-hook 'find-file-hook 'wrap-region-mode)

Try selecting a region and press on of the punctuation keys, for
example **"**, **{** or **(**.
