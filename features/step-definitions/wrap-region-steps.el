(Given "^\\(?:wrap-region is active\\|I enable wrap-region\\)$"
       (lambda ()
         (wrap-region-mode 1)))

(Given "^\\(?:wrap-region is inactive\\|I disable wrap-region\\)$"
       (lambda ()
         (wrap-region-mode -1)))

(Given "^I add wrapper \"\\(.+\\)\"$"
       (lambda (wrapper)
         (let ((split (split-string wrapper "/")))
           (wrap-region-add-wrapper (car split) (cadr split)))))

(Given "^I remove wrapper \"\\(.\\)\"$"
       (lambda (left)
         (wrap-region-remove-wrapper left)))

(Given "^I enable the global mode$"
       (lambda ()
         (wrap-region-global-mode 1)))

(Given "^I add \\(.+\\) as except mode$"
       (lambda (mode)
         (add-to-list 'wrap-region-except-modes (intern mode))))

(When "^I enable html-mode$"
      (lambda ()
        (html-mode)))

(When "^I enable text-mode$"
      (lambda ()
        (text-mode)))

(Given "^I add this mode hook$"
       (lambda (code)
         (add-hook 'wrap-region-hook
                   `(lambda ()
                      (espuds-fake-eval ,code)))))

(Given "^I add this before wrap hook$"
       (lambda (code)
         (add-hook 'wrap-region-before-wrap-hook
                   `(lambda ()
                      (espuds-fake-eval ,code)))))

(Given "^I add this after wrap hook$"
       (lambda (code)
         (add-hook 'wrap-region-after-wrap-hook
                   `(lambda ()
                      (espuds-fake-eval ,code)))))
