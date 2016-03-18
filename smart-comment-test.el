(require 'ert)
(require 'smart-comment)

(ert-deftest buff-test ()
  (with-temp-buffer
    (insert "
;; ❌ (test Iholo)
;; (commented)
")
    (setq comment-start ";")
    (setq comment-add 1)
    (search-backward "I")
    (delete-forward-char 1)
    (smart-comment 4)
    (should (equal "
;; ❌ ;; ❌ (test holo)
;; (commented)
" (buffer-string)))))

(ert-deftest smart-comment-test-uncomment-mark ()
  (with-temp-buffer
    (insert ";; ❌ (teIst)\n")
    (setq comment-start ";")
    (setq comment-add 1)
    (search-backward "I")
    (delete-forward-char 1)
    (smart-comment 2)
    (should (equal "(test)\n" (buffer-string)))))
