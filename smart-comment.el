;;; -*- lexical-binding: t; -*-

;; Smart comment

(setq smart-comment-end-action 'comment-dwim)
(setq smart-comment-beg-action 'smart-comment-mark-line)
(setq smart-comment-mid-action 'smart-comment-line)

(defun smart-comment-mark-string ()
  (concat comment-start
          (if (string= " " (substring comment-start -1)) "" " ")
          "DEL:"))

(defun smart-comment-cleanup ()
  "Remove lines marked for deletion"
  (interactive "*")
  (let ((search (smart-comment-mark-string)))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward (regexp-quote search) nil t)
        (kill-whole-line)))))

(defun smart-comment-mark-line (arg)
  "Mark a line for deletion"
  (interactive "*P")
  (smart-comment-line 4))

(defun smart-comment-mark-region (beg end arg)
  "Mark a region for deletion"
  (interactive "*r\nP")
  (let ((orig-comment-start comment-start))
    (setq comment-start (smart-comment-mark-string))
    (comment-or-uncomment-region beg end)
    (setq comment-start orig-comment-start)))

(defun smart-comment-is-at-beg ()
  (let ((orig-point (point)))
    (save-excursion (back-to-indentation) (<= orig-point (point)))))

(defun smart-comment-region (beg end arg)
  "Comment or uncomment a region"
  (interactive "*r\nP")
  (if (= 4 (prefix-numeric-value arg)) (smart-comment-mark-region beg end arg)
    (comment-or-uncomment-region beg end arg)))

(defun smart-comment-line (arg)
  "Comment or uncomment a line"
  (interactive "*P")
  (smart-comment-region (line-beginning-position) (line-end-position) arg))

(defun smart-comment (arg)
  "Smart commenting"
  (interactive "*P")
  (cond ((and mark-active transient-mark-mode)
         (smart-comment-region (region-beginning) (region-end) arg))
        ((looking-at "\\s-*$")
         (funcall smart-comment-eol-action arg))
        ((smart-comment-is-at-beg)
         (funcall smart-comment-beg-action arg))
        (t (funcall smart-comment-mid-action arg))))

(provide 'smart-comment)
