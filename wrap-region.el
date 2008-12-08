;;; wrap-region.el --- Wrap text with punctation or markup tag.

;; Copyright 2008  Johan Andersson

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; License ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Vocabulary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Punctuation - Is everything in written language other than the
;; actual letters or numbers ([, (, ., ", etc...).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;; Description ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Wrap region is a minor mode that wraps a region. That of course
;; only happens when there is a region selected in the buffer. If no
;; region is selected that punctuation is inserted. And if
;; `wrap-region-insert-twice' is set to t, the corresponding
;; punctuation is inserted as weel, and the cursor is placed between
;; them. An exception to this is if `wrap-region-tag-active' is set
;; to t. Then "<" will be interped as a markup tag (<tag>...</tag>),
;; and that tag will wrap the region.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;; Installation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; To use this mode you first have to make sure that this file is in
;; your load-path variable:
;; (add-to-list 'load-path "/path/to/directory/or/file")
;;
;; Then require it:
;; (require 'wrap-region)
;;
;; Then start it:
;; (wrap-region-mode t) or M-x wrap-region-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Commentary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; First of all you want to activate this mode for all major modes
;; you want to use this in:
;;
;; (add-hook 'ruby-mode-hook
;;           '(lambda()
;;              (wrap-region-mode t)
;;              ))
;;
;; By only doing this you will activate all default punctuations. But
;; you may not want some punctuation to be used for a certain mode. If
;; That is the case you want to use `wrap-region-set-mode-punctuations'.
;;
;; (add-hook 'ruby-mode-hook
;;           '(lambda()
;;              (wrap-region-set-mode-punctuations '("\"" "'" "("))
;;              (wrap-region-mode t)
;;              ))
;;
;; This will activate the punctuations ", ', and ( only, and it will be
;; activated for ruby-mode.
;;
;; You can also pass a major mode to this function if you want to set
;; all mode specific punctuations at the same place:
;;
;; (wrap-region-set-mode-punctuations '("\"" "'" "(") 'ruby-mode)
;; (wrap-region-set-mode-punctuations '("[" "{" "(") 'java-mode)
;;
;;
;; You can also customize if you want to insert one or two punctuations
;; (and then move in between them) if there is no region selected.
;; This is configured by the variable `wrap-region-insert-twice'.
;; t means to insert two punctuations and then move in between them,
;; and nil means to only insert that punctuation.
;;
;; Insert both and move in between:
;; (setq wrap-region-insert-twice t)
;;
;; Insert punctuation only:
;; (setq wrap-region-insert-twice nil)
;;
;;
;; If noting is said, "<" will be used as a regular punctuation with
;; ">" as it's corresponding. This is desirable in languages such as
;; Java where this is syntax is used:
;; Set<Object> set = new HashSet<Object>();
;;
;; But in markup languages, such as x(HTML), XML, etc... you use tags
;; and want to make use of the tags functionality. That is controlled
;; by the variable `wrap-region-tag-active'. By setting this to t,
;; when pressing "<" you will be prompted to enter a tag, which you
;; can do in two ways.
;;
;; The first is to enter some tag such as "div". The selected region
;; will then be wrapped with the div tag:
;; <div>selected region</div>
;;
;; The second way is to also enter attributes for the tag, such as
;; class, id, name, etc... If you enter:
;; div class="some_class" id="some_id"
;; you will end up with this:
;; <div class="some_class" id="some_id">selected region</div>
;;
;;
;; This mode comes with some default punctuations
;; (see `wrap-region-punctuations-table'). This might not always be
;; enough. And there's where `wrap-region-add-punctuation' comes
;; in handy. As an example we add # as a punctuation and # as it's
;; corresponding punctuation:
;; (wrap-region-add-punctuation "#" "#")
;;
;; Note that even if you use `wrap-region-set-mode-punctuations'
;; for mode specific punctuations, you still need to use
;; `wrap-region-add-punctuation'. This is because that's how the
;; corresponding punctuation is found.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; History ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DATE          VERSION    UPDATES/CHANGES
;; 2008-12-07    0.0.1      First version released.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst wrap-region-version "0.0.1"
  "Wrap region version. This is how the version numbering works:
MAJOR_FEATURE_UPDATE.MINOR_FEATURE_UPDATE.BUGFIX")

(defvar wrap-region-mode-map (make-sparse-keymap)
  "Keymap for `wrap-region-mode'.")

(defvar wrap-region-punctuations-table (make-hash-table :test 'equal)
  "A list with all possible punctuations and their right corresponding punctuation.")

(puthash "\"" "\"" wrap-region-punctuations-table)
(puthash "'"  "'"  wrap-region-punctuations-table)
(puthash "("  ")"  wrap-region-punctuations-table)
(puthash "{"  "}"  wrap-region-punctuations-table)
(puthash "["  "]"  wrap-region-punctuations-table)
(puthash "<"  ">"  wrap-region-punctuations-table)
(puthash "|"  "|"  wrap-region-punctuations-table)
(puthash "\\" "\\" wrap-region-punctuations-table)

(defvar wrap-region-tag-active nil
  "This variable tells whether < are to be used
as a tag or a regular punctuation.")
(make-variable-buffer-local 'wrap-region-tag-active)

(defvar wrap-region-mode-punctuations (make-hash-table)
  "Use this if you want mode specific punctuations.
Key is the symbol name of the major mode and the value is a list
of punctuations.")

(defvar wrap-region-insert-twice t
  "If this is true, when inserting a punctuation,
the corresponding punctuation will be inserted after and
the cursor will be placed between them.")

(defvar wrap-region-before-hook '()
  "Evaluated before the region is wrapped.
Two variables are available in the hook:
wrap-region-beginning which is the beginning of the region
and wrap-region-end which is the end of the region.")

(defvar wrap-region-after-hook '()
  "Evaluated after the region is wrapped.
Two variables are available in the hook:
wrap-region-beginning which is the beginning of the region
and wrap-region-end which is the end of the region.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  "Wraps a region with a tag if any region is selected.
Otherwise the punctuation(s) are inserted."
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
  "Inserts LEFT or LEFT and it's corresponding punctuation
if `wrap-region-insert-twice' is set to t."
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
  "Returns the corresponding punctuation to the given punctuation
or nil if the punctuation does not exists."
  (gethash punctuation wrap-region-punctuations-table))

(defun wrap-region-add-punctuation (left right)
  "Adds a new punctuation pair to the punctuation list."
  (puthash left right wrap-region-punctuations-table))

(defun wrap-region-set-mode-punctuations (punctuations &optional mode)
  "Use this when the punctuations should be
customized depending on the major mode. MODE argument
is optional and will be set to `major-mode' as default."
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
;;;###autoload

(provide 'wrap-region)

;;; wrap-region.el ends here