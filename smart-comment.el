;; Smart comment

(setq smart-comment-end-action 'comment-dwim)
(setq smart-comment-beg-action 'smart-comment-and-mark-line)
(setq smart-comment-mid-action 'smart-comment-or-uncomment-line)

(defun smart-comment-cleanup ()
  "Remove lines marked for deletion"
  (interactive)
  (let ((search (concat comment-start
                        (if (string= " " (substring comment-start -1)) "DEL:" " DEL:"))))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward (regexp-quote search) nil t)
        (kill-whole-line)))))

(defun smart-comment-or-uncomment-line (arg)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun smart-comment-and-mark-line (arg)
  (back-to-indentation)
  (let ((ends-with-space (string= " " (substring comment-start -1)))
        (orig-comment-start comment-start))
    (setq comment-start (concat orig-comment-start
                                (if ends-with-space "DEL:" " DEL:")))
    (smart-comment-or-uncomment-line arg)
    (setq comment-start orig-comment-start)))

(defun smart-comment (arg)
  "Smart commenting"
  (interactive "*P")
  (cond ((looking-at "\\s-*$")
         (funcall smart-comment-eol-action arg))
        ((let ((orig-point (point)))
           (save-excursion (back-to-indentation) (<= orig-point (point))))
         (funcall smart-comment-beg-action arg))
        (t (funcall smart-comment-mid-action arg))))

(provide 'smart-comment)
