;;; wrap-region.el --- Wrap text with punctation or tag

;; Copyright (C) 2008-2010 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.1.3
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

;; wrap-region is a minor mode that wraps a region with
;; punctuations. For tagged markup modes, such as HTML and XML, it
;; wraps with tags.
;;
;; To use wrap-region, make sure that this file is in Emacs load-path:
;;   (add-to-list 'load-path "/path/to/directory/or/file")
;;
;; Then require wrap-region:
;;   (require 'wrap-region)
;;
;;
;; To start wrap-region:
;;   (wrap-region-mode t) or M-x wrap-region-mode
;;
;; If you only want wrap-region active in some mode, use a hook:
;;   (add-hook 'ruby-mode-hook 'wrap-region-mode)
;;
;; Or if you want to activate it in all buffers, use the global mode:
;;   (wrap-region-global-mode t)
;;
;;
;; To wrap a region, select that region and hit one of the punctuation
;; keys. In "tag-modes" (html-mode, sgml-mode or xml-mode), "<" is
;; replaced and wraps the region with a tag. To activate this behavior
;; in a mode other than default, you do:
;;   (add-to-list 'wrap-region-tag-active-modes 'some-tag-mode)
;;
;; `wrap-region-punctuations-table' contains a few default
;; punctuations that wraps. You can add you own like this:
;;   (wrap-region-add-punctuation "#" "#")
;;
;; Wrap Region stores a list (`wrap-region-except-modes') of modes in
;; which `wrap-region-mode' should not be activated in (note, only if
;; you use the global mode) because of conflicting key bindings.
;;
;; You can add new except modes like this:
;;   (add-to-list 'wrap-region-except-modes 'conflicting-mode)


;;; Code:

(defvar wrap-region-insert-twice nil
  "If this is non nil, when inserting a punctuation, the corresponding
punctuation will be inserted after and the cursor will be placed
between them.")

(defvar wrap-region-mode-map (make-sparse-keymap)
  "Keymap for `wrap-region-mode'.")

(defvar wrap-region-punctuations-table
  (let ((table (make-hash-table :test 'equal)))
    (puthash "\"" "\"" table)
    (puthash "'"  "'"  table)
    (puthash "("  ")"  table)
    (puthash "{"  "}"  table)
    (puthash "["  "]"  table)
    (puthash "<"  ">"  table)
    table)
  "A map with all punctuations and their right corresponding punctuation.")

(defvar wrap-region-tag-active-modes '(html-mode sgml-mode rhtml-mode)
  "List of modes where < should be used as a tag instead of a regular punctuation.")

(defvar wrap-region-hook '()
  "Called when `wrap-region-mode' is started.")

(defvar wrap-region-before-insert-twice-hook '()
  "Called before insert twice has been done.")

(defvar wrap-region-after-insert-twice-hook '()
  "Called after insert twice has been done.")

(defvar wrap-region-before-wrap-hook '()
  "Called before wrapping has been done.")

(defvar wrap-region-after-wrap-hook '()
  "Called after wrapping has been done.")

(defvar wrap-region-state-active nil
  "t if insert twice is active. nil otherwise.")

(defvar wrap-region-state-pos nil
  "The position when insert twice was last activated. nil if not active.")

(defvar wrap-region-except-modes '(calc-mode dired-mode)
  "A list of modes in which `wrap-region-mode' should not be activated.")


(defun wrap-region-with-punctuation-or-insert ()
  "Wraps region if any. Otherwise insert punctuations."
  (interactive)
  (let ((key (char-to-string last-input-event)))
    (if (and mark-active (wrap-region-right-buddy key))
        (if (and (member major-mode wrap-region-tag-active-modes) (string= key "<"))
            (wrap-region-with-tag)
          (wrap-region key (wrap-region-right-buddy key) (region-beginning) (region-end)))
      (wrap-region-insert key))))

(defun wrap-region (left right beg end)
  "Wraps region from BEG to END with LEFT and RIGHT."
  (run-hooks 'wrap-region-before-wrap-hook)
  (save-excursion
    (goto-char beg)
    (insert left)
    (goto-char (+ end (length left)))
    (insert right))
  (run-hooks 'wrap-region-after-wrap-hook))

(defun wrap-region-insert (left)
  "Inserts LEFT and its right buddy if `wrap-region-insert-twice' is non nil."
  (if wrap-region-insert-twice
      (wrap-region-insert-twice left)
    (insert left)))

(defun wrap-region-insert-twice (left)
  "Inserts LEFT and its right buddy."
  (cond ((and wrap-region-state-active (wrap-region-match left))
         (forward-char 1)
         (wrap-region-reset))
        (t
         (run-hooks 'wrap-region-before-insert-twice-hook)
         (insert left)
         (when (wrap-region-right-buddy left)
           (save-excursion
             (insert (wrap-region-right-buddy left)))
           (run-hooks 'wrap-region-after-insert-twice-hook)
           (wrap-region-activate)))))

(defun wrap-region-with-tag ()
  "Wraps region with tag."
  (let* ((tag (read-string "Enter Tag (with optional attributes): "))
         (split (split-string tag " "))
         (tag-name (car split))
         (left (concat "<" tag ">"))
         (right (concat "</" tag-name ">")))
    (wrap-region left right (region-beginning) (region-end))))

(defun wrap-region-right-buddy (left)
  "Returns right buddy to LEFT."
  (gethash left wrap-region-punctuations-table))

(defun wrap-region-add-punctuation (left right)
  "Adds a new punctuation pair."
  (puthash left right wrap-region-punctuations-table))

(defun wrap-region-match (key)
  "Returns t if the current position is an enclosing match with
KEY. nil otherwise."
  (let ((before (char-to-string (char-before)))
        (after  (char-to-string (char-after))))
    (and (string= key after)
         (string= after (wrap-region-right-buddy before)))))

(defun wrap-region-reset ()
  "Set insert twice to inactive."
  (setq wrap-region-state-pos nil)
  (setq wrap-region-state-active nil))

(defun wrap-region-activate ()
  "Set insert twice to active at current point."
  (setq wrap-region-state-pos (point))
  (setq wrap-region-state-active t))

(defun wrap-region-command ()
  "Called after a command is executed.
If the executed command moved the cursor, then insert twice is set inactive."
  (if (and wrap-region-state-active
           (/= (point) wrap-region-state-pos))
      (wrap-region-reset)))

(defun wrap-region-backward-delete-char ()
  "Deletes an enclosing pair backwards if insert twice is active.
 Otherwise it falls back to default."
  (interactive)
  (cond ((and wrap-region-state-active (wrap-region-match (char-to-string (char-after))))
         (forward-char 1)
         (backward-delete-char 2))
        (t
         (let ((wrap-region-mode nil)
               (original-func (key-binding (kbd "DEL"))))
           (call-interactively original-func))))
  (wrap-region-reset))

(defun wrap-region-define-keys ()
  "Defines all key bindings."
  (if wrap-region-insert-twice
      (define-key wrap-region-mode-map (kbd "DEL") 'wrap-region-backward-delete-char))
  (maphash (lambda (left right)
             (define-key wrap-region-mode-map left 'wrap-region-with-punctuation-or-insert)
             (if wrap-region-insert-twice
                 (define-key wrap-region-mode-map right 'wrap-region-with-punctuation-or-insert)))
           wrap-region-punctuations-table))

;;;###autoload
(define-minor-mode wrap-region-mode
  "Wrap region with stuff."
  :init-value nil
  :lighter " wr"
  :keymap wrap-region-mode-map
  (cond (wrap-region-mode
         (wrap-region-define-keys)
         (wrap-region-reset)
         (add-hook 'post-command-hook 'wrap-region-command)
         (run-hooks 'wrap-region-hook))
        (t
         (remove-hook 'post-command-hook 'wrap-region-command))))

;;;###autoload
(defun turn-on-wrap-region-mode ()
  "Turn on `wrap-region-mode'"
  (interactive)
  (unless (member major-mode wrap-region-except-modes)
    (wrap-region-mode +1)))

;;;###autoload
(defun turn-off-wrap-region-mode ()
  "Turn off `wrap-region-mode'"
  (interactive)
  (wrap-region-mode -1))

;;;###autoload
(define-globalized-minor-mode wrap-region-global-mode
  wrap-region-mode
  turn-on-wrap-region-mode)

(provide 'wrap-region)

;;; wrap-region.el ends here
