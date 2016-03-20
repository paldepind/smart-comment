(require 'ert)
(require 'smart-comment)

(ert-deftest smart-comment-remove-mark ()
  "If commented line has mark mark is removed"
  (with-temp-buffer
    (insert "
;; ❌ (testI)
;; (commented)
")
    (setq comment-start ";")
    (setq comment-add 1)
    (search-backward "I")
    (delete-forward-char 1)
    (smart-comment 4)
    (should (equal "
;; (test)
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

(ert-deftest smart-comment-test-add-only-mark ()
  "When M-u M-; is invoked in commented line
   only a marker should be added"
  (with-temp-buffer
    (insert ";; (teIst)\n")
    (setq comment-start ";")
    (setq comment-add 1)
    (search-backward "I")
    (delete-forward-char 1)
    (smart-comment 4)
    (should (equal ";; ❌ (test)\n" (buffer-string)))))

(ert-deftest smart-comment-test-add-only-mark ()
  "When M-u M-; is invoked in commented line
   only a marker should be added"
  (with-temp-buffer
    (insert ";; (oneI)\n;; ❌ (two)")
    (setq comment-start ";")
    (setq comment-add 1)
    (search-backward "I")
    (delete-forward-char 1)
    (smart-comment 2)
    (should (equal "(one)\n;; ❌ (two)" (buffer-string)))))

(ert-deftest smart-comment-test-toggle-mark-region ()
  "Add marks if not all lines are marked")
