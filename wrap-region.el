;;; wrap-region.el --- Wrap text with punctation or tag

;; Copyright (C) 2008 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: speed, convenience
;; URL: http://github.com/rejeep/wrap-region

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; wrap-region is a minor mode that wraps text with punctuations. For
;; some tagged markup modes, such as HTML and XML, it wraps a region
;; with a tag instead of a punctuation.

;; To use wrap-region, make sure that this file is in Emacs load-path
;; (add-to-list 'load-path "/path/to/directory/or/file")
;;
;; Then require wrap-region
;; (require 'wrap-region)

;; To start wrap-region
;; (wrap-region-mode t) or M-x wrap-region-mode
;;
;; wrap-region is buffer local, so hook it up.
;; (add-hook 'ruby-mode-hook 'wrap-region-mode)
;;
;; This will enable all of wrap-region's default punctuations in
;; ruby-mode. To enable only a few punctuations, use
;; `wrap-region-set-mode-punctuations'.
;; (add-hook 'ruby-mode-hook
;;           '(lambda()
;;              (wrap-region-set-mode-punctuations '("\"" "'" "("))
;;              (wrap-region-mode t)))
;;
;; You can also pass a major mode to this function if you want to set
;; all mode specific punctuations at the same place:
;;
;; (wrap-region-set-mode-punctuations '("\"" "'" "(") 'ruby-mode)
;; (wrap-region-set-mode-punctuations '("[" "{" "(") 'java-mode)

;; If no region is selected so there's nothing to wrap, wrap-region
;; will insert that punctuation, unless if `wrap-region-insert-twice'
;; set to t. In this case two punctuations will be inserted and the
;; cursor placed in between them.

;; By default "<" is used as a regular punctuation with ">" as it's
;; corresponding. This is probably the wanted behavior in languages
;; such as Java, where this is syntax can be used:
;; Set<String> set = new HashSet<String>();
;;
;; But in markup languages, such as HTML and XML, you use tags and
;; want to make use of the tags functionality. To enable it, set
;; `wrap-region-tag-active' to t before activating wrap-region.
;; (add-hook 'rhtml-mode-hook
;;           '(lambda()
;;              (setq wrap-region-tag-active t)
;;              (wrap-region-mode t)))
;;
;; You can now wrap a region with a tag. When asked for the tag, you
;; may include attributes, such as class or id.

;; wrap-region comes with a few default punctuations (see
;; `wrap-region-punctuations-table'). You can add you own punctuations
;; using `wrap-region-add-punctuation'.
;; (wrap-region-add-punctuation "#" "#")

;;; Code:

(defcustom wrap-region-insert-twice nil
  "If this is true, when inserting a punctuation,
the corresponding punctuation will be inserted after and
the cursor will be placed between them."
  :group 'wrap-region)

(defvar wrap-region-mode-map (make-sparse-keymap)
  "Keymap for `wrap-region-mode'.")

(defvar wrap-region-punctuations-table (make-hash-table :test 'equal)
  "A list with all possible punctuations and their right
  corresponding punctuation.")

(puthash "\"" "\"" wrap-region-punctuations-table)
(puthash "'"  "'"  wrap-region-punctuations-table)
(puthash "("  ")"  wrap-region-punctuations-table)
(puthash "{"  "}"  wrap-region-punctuations-table)
(puthash "["  "]"  wrap-region-punctuations-table)
(puthash "<"  ">"  wrap-region-punctuations-table)
(puthash "|"  "|"  wrap-region-punctuations-table)
(puthash "\\" "\\" wrap-region-punctuations-table)

(defvar wrap-region-tag-active nil
  "This variable tells whether < are to be used as a tag or a regular
punctuation.")
(make-variable-buffer-local 'wrap-region-tag-active)

(defvar wrap-region-mode-punctuations (make-hash-table)
  "Use this if you want mode specific punctuations.  Key is the symbol
name of the major mode and the value is a list of punctuations.")

(defvar wrap-region-before-hook '()
  "Evaluated before the region is wrapped.  Two variables are
available in the hook: wrap-region-beginning which is the beginning of
the region and wrap-region-end which is the end of the region.")

(defvar wrap-region-after-hook '()
  "Evaluated after the region is wrapped.
Two variables are available in the hook:
wrap-region-beginning which is the beginning of the region
and wrap-region-end which is the end of the region.")

(defun wrap-region-with-punctuation-or-insert (left)
  "Wraps a region if any, else inserts the punctuation(s)."
  (interactive)
  (if mark-active
      (wrap-region left (wrap-region-corresponding-punctuation left) (region-beginning) (region-end))
    (wrap-region-insert left)))

(defun wrap-region-with-punctuations (left right)
  "Wraps a region with LEFT and RIGHT."
  (wrap-region left right (region-beginning) (region-end)))

(defun wrap-region-with-tag-or-insert ()
  "Wraps a region with a tag if any region is selected.  Otherwise the
punctuation(s) are inserted."
  (interactive)
  (if mark-active
      (call-interactively 'wrap-region-with-tag)
    (wrap-region-insert "<")))

(defun wrap-region-with-tag (tag)
  "Wraps a region with a tag."
  (interactive "*sTag (with optional attributes): ")
  (let* ((elements (split-string tag " "))
         (tag-name (car elements))
         (tag-right (concat "</" tag-name ">"))
         (tag-left (concat "<" (if (= (length elements) 1) tag-name tag) ">")))
    (wrap-region tag-left tag-right (region-beginning) (region-end))))

(defun wrap-region-insert (left)
  "Inserts LEFT or LEFT and it's corresponding punctuation if
`wrap-region-insert-twice' is set to t."
  (insert left)
  (cond (wrap-region-insert-twice
         (insert (wrap-region-corresponding-punctuation left))
         (backward-char))))

(defun wrap-region (left right beg end)
  "Wraps region with LEFT and RIGHT."
  (let ((wrap-region-beginning beg) (wrap-region-end end))
    (run-hooks 'wrap-region-before-hook)
    (save-excursion
      (goto-char beg)
      (insert left)
      (goto-char (+ end (length left)))
      (insert right))
    (run-hooks 'wrap-region-after-hook)))

(defun wrap-region-corresponding-punctuation (punctuation)
  "Returns the corresponding punctuation to the given punctuation or
nil if the punctuation does not exists."
  (gethash punctuation wrap-region-punctuations-table))

(defun wrap-region-add-punctuation (left right)
  "Adds a new punctuation pair to the punctuation list."
  (puthash left right wrap-region-punctuations-table))

(defun wrap-region-set-mode-punctuations (punctuations &optional mode)
  "Use this when the punctuations should be customized depending on
the major mode. MODE argument is optional and will be set to
`major-mode' as default."
  (puthash (or mode major-mode) punctuations wrap-region-mode-punctuations))

;;;###autoload
(define-minor-mode wrap-region-mode
  "Wrap region with punctuations or insert."
  :init-value nil
  :lighter " wr"
  :keymap wrap-region-mode-map
  (if wrap-region-mode
      (let ((punctuations (gethash major-mode wrap-region-mode-punctuations)))
        (unless punctuations
          (maphash (lambda (k v) (add-to-list 'punctuations k)) wrap-region-punctuations-table))
        (dolist (key punctuations)
          (define-key wrap-region-mode-map key `(lambda () (interactive) (wrap-region-with-punctuation-or-insert ,key))))
        (if wrap-region-tag-active
            (define-key wrap-region-mode-map "<" 'wrap-region-with-tag-or-insert)))))

(provide 'wrap-region)

;;; wrap-region.el ends here
