(defun parse-modes (mode-or-modes)
  (let ((mode-split (split-string mode-or-modes ",")))
    (if (> (length mode-split) 1)
        (mapcar 'intern mode-split)
      (intern mode-or-modes))))


(Given "^\\(?:wrap-region is active\\|I enable wrap-region\\)$"
       (lambda ()
         (wrap-region-mode 1)))

(Given "^\\(?:wrap-region is inactive\\|I disable wrap-region\\)$"
       (lambda ()
         (wrap-region-mode -1)))

(Given "^I add wrapper \"\\([^\"]+\\)\"$"
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

(Then "^the buffer should be empty$"
      (lambda ()
        (should (zerop (length (espuds-buffer-contents))))))

(Given "^I add wrapper \"\\(.+\\)\" for \"\\(.+\\)\"$"
       (lambda (wrapper mode-or-modes)
         (let* ((modes (parse-modes mode-or-modes))
                (wrapper-split
                 (split-string wrapper "/"))
                (args
                 (append
                  wrapper-split
                  (if (= (length wrapper-split) 2)
                      (list nil modes)
                    (list modes)))))
           (apply 'wrap-region-add-wrapper args))))

(Given "^I remove wrapper \"\\(.+\\)\" from \"\\(.+\\)\"$"
       (lambda (key mode-or-modes)
         (let ((modes (parse-modes mode-or-modes)))
           (wrap-region-remove-wrapper key modes))))
