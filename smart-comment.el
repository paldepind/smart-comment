;; Smart comment

(defun smart-comment-cleanup ()
  "Remove lines marked for deletion"
  (interactive)
  (let ((search (concat comment-start
                        (if (string-match " \'" comment-start) "DEL:" " DEL:"))))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward (regexp-quote search) nil t)
        (kill-whole-line)))))

(defun smart-comment-or-uncomment-line ()
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun smart-comment-and-mark-line ()
  (back-to-indentation)
  (let ((ends-with-space (string= " " (substring comment-start -1)))
        (orig-comment-start comment-start))
    (setq comment-start (concat orig-comment-start
                                (if ends-with-space "DEL:" " DEL:")))
    (smart-comment-or-uncomment-line)
    (setq comment-start orig-comment-start)))

(defun smart-comment (arg)
  "Smart commenting"
  (interactive "*P")
  (cond ((looking-at "\\s-*$")
         (comment-dwim arg))
        ((let ((orig-point (point)))
           (save-excursion (back-to-indentation) (<= orig-point (point))))
         (smart-comment-and-mark-line))
        (t (smart-comment-or-uncomment-line))))

(provide 'smart-comment)
