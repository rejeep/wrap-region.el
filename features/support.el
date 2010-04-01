(add-to-list 'load-path "~/dev/wrap-region")
(require 'wrap-region)

(add-to-list 'load-path "~/dev/espuds")
(require 'espuds)

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

 ;; Clear all except modes
 (setq wrap-region-except-modes '())
 )
