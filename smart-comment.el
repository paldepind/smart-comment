;;; -*- lexical-binding: t; -*-

;; Smart comment

(setq smart-comment-end-action 'comment-dwim)
(setq smart-comment-beg-action 'smart-comment-mark-line)
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
  "Comment or uncomment a line"
  (interactive "*P")
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun smart-comment-mark-line (arg)
  "Mark a line for deletion"
  (interactive "*P")
  (back-to-indentation)
  (let ((ends-with-space (string= " " (substring comment-start -1)))
        (orig-comment-start comment-start))
    (setq comment-start (concat orig-comment-start
                                (if ends-with-space "DEL:" " DEL:")))
    (smart-comment-or-uncomment-line arg)
    (setq comment-start orig-comment-start)))

(defun smart-comment-mark-region (arg)
  "Mark a region for deletion"
  (interactive "*P")
  (let ((ends-with-space (string= " " (substring comment-start -1)))
        (orig-comment-start comment-start))
    (setq comment-start (concat orig-comment-start
                                (if ends-with-space "DEL:" " DEL:")))
    (comment-or-uncomment-region (region-beginning) (region-end))
    (setq comment-start orig-comment-start)))

(defun smart-comment-is-at-beg ()
  (let ((orig-point (point)))
    (save-excursion (back-to-indentation) (<= orig-point (point)))))

(defun smart-comment-region (arg)
  "Comment or uncomment a region"
  (interactive "*P")
  (message "%s" (prefix-numeric-value arg))
  (if (= 4 (prefix-numeric-value arg)) (smart-comment-mark-region arg)
    (comment-or-uncomment-region (region-beginning) (region-end) arg)))

(defun smart-comment (arg)
  "Smart commenting"
  (interactive "*P")
  (cond ((and mark-active transient-mark-mode)
         (smart-comment-region arg))
        ((looking-at "\\s-*$")
         (funcall smart-comment-eol-action arg))
        ((smart-comment-is-at-beg)
         (funcall smart-comment-beg-action arg))
        (t (funcall smart-comment-mid-action arg))))

(provide 'smart-comment)
