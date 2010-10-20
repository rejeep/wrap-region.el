(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq enclose-root-path project-directory)
  (setq enclose-util-path (expand-file-name "util" project-directory)))

(add-to-list 'load-path enclose-root-path)
(add-to-list 'load-path (expand-file-name "espuds" enclose-util-path))
(add-to-list 'load-path (expand-file-name "emacs-lisp" (expand-file-name "lisp" (expand-file-name "ert" enclose-util-path))))

(require 'wrap-region)
(require 'espuds)
(require 'ert)

(setq default-except-modes wrap-region-except-modes)

(Before
 ;; Start with a clean slate.
 (switch-to-buffer (get-buffer-create "*wrap-region*"))
 (erase-buffer)
 (transient-mark-mode 1)
 (deactivate-mark))

(After
 ;; For scenarios that add and remove wrapper.
 (wrap-region-add-wrapper "(" ")")
 (wrap-region-remove-wrapper "`")

 ;; Reset hooks
 (setq wrap-region-hook nil)
 (setq wrap-region-before-wrap-hook nil)
 (setq wrap-region-after-wrap-hook nil)

 ;; Disable wrap-region-mode
 (wrap-region-mode -1)
 (wrap-region-global-mode -1)

 ;; Reset all except modes
 (setq wrap-region-except-modes default-except-modes))
