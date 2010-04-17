(let ((current-directory (file-name-directory load-file-name)))
  (setq wrap-region-root-path (expand-file-name ".." current-directory))
  (setq wrap-region-util-path (expand-file-name "util" wrap-region-root-path)))

(add-to-list 'load-path wrap-region-root-path)
(add-to-list 'load-path (expand-file-name "espuds" wrap-region-util-path))
(add-to-list 'load-path (expand-file-name "ert" wrap-region-util-path))

(require 'wrap-region)
(require 'espuds)
(require 'ert)

(setq default-except-modes wrap-region-except-modes)

(After
 (setq wrap-region-insert-twice nil)

 ;; Scenario "Add punctuation" add # to the table
 (remhash "#" wrap-region-punctuations-table)

 ;; Reset hooks
 (setq wrap-region-hook '())
 (setq wrap-region-before-insert-twice-hook '())
 (setq wrap-region-after-insert-twice-hook '())
 (setq wrap-region-before-wrap-hook '())
 (setq wrap-region-after-wrap-hook '())

 ;; Disable global mode
 (wrap-region-global-mode -1)

 ;; Reset all except modes
 (setq wrap-region-except-modes default-except-modes)
 )
