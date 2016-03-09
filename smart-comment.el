;; Smart comment

(defun smart-comment-cleanup ()
  "Remove lines marked for deletion"
  (interactive)
  (beginning-of-buffer)
  (while (re-search-forward (regexp-quote (concat comment-start "DEL:")) nil t)
    (kill-whole-line)))

(defun smart-comment-or-uncomment-line ()
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun smart-comment-and-mark-line ()
  (back-to-indentation)
  (insert "DEL: ")
  (smart-comment-or-uncomment-line))

(defun smart-comment (arg)
  "Smart commenting"
  (interactive "*P")
  (message "%s" arg)
  (if (looking-at "\\s-*$")
      (comment-dwim arg)
    (if (let ((orig-point (point)))
          (save-excursion (back-to-indentation) (<= orig-point (point))))
        (smart-comment-and-mark-line)
      (smart-comment-or-uncomment-line))))
