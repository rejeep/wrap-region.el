(Given "^I \\(enable\\|disable\\) wrap-region$"
       (lambda (status)
         (if (string= status "enable")
             (turn-on-wrap-region-mode)
           (turn-off-wrap-region-mode))))

(Given "^insert twice is \\(active\\|inactive\\)$"
       (lambda (status)
         (setq wrap-region-insert-twice (string= status "active"))))

(Given "^I start html-mode$"
       (lambda ()
         (html-mode)))
