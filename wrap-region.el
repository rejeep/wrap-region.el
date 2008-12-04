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

;;;;;;;;;;;;;;;;;;;;;;;;;;; Installation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Commentary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; History ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst wrap-region-version ""
  "")

(defvar wrap-region-mode-map (make-sparse-keymap)
  "")

(defvar wrap-region-punctuations-table ""
  "")

(defvar wrap-region-tag-active t
  "")
(make-variable-buffer-local 'wrap-region-tag-active)

(defvar wrap-region-punctuations ""
  "")
(make-variable-buffer-local 'wrap-region-punctuations)

(defvar wrap-region-before-hook '()
  "")

(defvar wrap-region-after-hook '()
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wrap-region-with-punctuation-or-insert ()
  ""
  )

(defun wrap-region-with-punctuation ()
  ""
  )

(defun wrap-region-with-tag-or-insert ()
  ""
  )

(defun wrap-region-with-tag ()
  ""
  )

(defun wrap-region ()
  ""
  )

(defun wrap-region-corresponding-punctuation ()
  ""
  )

(defun wrap-region-add-punctuation ()
  ""
  )

;;;###autoload
(define-minor-mode wrap-region
  ""
  :init-value nil
  :lighter " wrap-region"
  :keymap '())
;;;###autoload

(provide 'wrap-region)

;;; wrap-region.el ends here