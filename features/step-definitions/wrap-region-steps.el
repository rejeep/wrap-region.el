(Given "^\\(?:wrap-region is active\\|I enable wrap-region\\)$"
       (lambda ()
         (wrap-region-mode 1)))

(Given "^\\(?:wrap-region is inactive\\|I disable wrap-region\\)$"
       (lambda ()
         (wrap-region-mode -1)))

(Given "^I add wrapper \"\\(.+\\)\"$"
       (lambda (wrapper)
         (apply 'wrap-region-add-wrapper (split-string wrapper "/"))))

(Given "^I remove wrapper \"\\(.\\)\"$"
       (lambda (left)
         (wrap-region-remove-wrapper left)))

(Given "I enable wrap-region globaly"
       (lambda ()
         (wrap-region-global-mode 1)))

(Given "^I add \\(.+\\) as an except mode$"
       (lambda (mode)
         (add-to-list 'wrap-region-except-modes (intern mode))))

(When "^I enable \\(.+\\)-mode$"
      (lambda (mode)
        (funcall (intern (format "%s-mode" mode)))))

(Then "the buffer should be empty"
      (lambda ()
        (should (zerop (length (espuds-buffer-contents))))))
