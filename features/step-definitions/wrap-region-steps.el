(Given "^insert twice is turned \"\\(on\\|off\\)\"$"
       (lambda (status)
         (setq wrap-region-insert-twice (string= status "on"))))

(Given "^wrap-region tag is \"\\(active\\|inactive\\)\"$"
       (lambda (status)
         (setq wrap-region-tag-active (string= status "active"))))

(Given "^the following before hook:$"
       (lambda (contents)
         (add-hook 'wrap-region-before-hook `(lambda() (espuds-fake-eval ,contents)))))

(Given "^the following after hook:$"
       (lambda (contents)
         (add-hook 'wrap-region-after-hook `(lambda() (espuds-fake-eval ,contents)))))
