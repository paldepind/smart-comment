;;; -*- lexical-binding: t; -*-

;; Smart comment

(defgroup smart-comment nil
  "Smarter commenting."
  :prefix "smart-comment-"
  :group 'fill)

(defcustom smart-comment-beg-action 'smart-comment-line
  "Function to call when commenting at beginning of a line."
  :type 'function)

(defcustom smart-comment-mid-action 'smart-comment-line
  "Function to call when commenting in the middle of a line."
  :type 'function)

(defcustom smart-comment-end-action 'comment-dwim
  "Function to call when commenting at end of line."
  :type 'function)

(defcustom smart-comment-mark-string "❌"
  "String used to indicate that a line is marked for removal"
  :type 'string)

(defun smart-comment-mark ()
  (concat comment-start
          (if (string= " " (substring comment-start -1)) "" " ")
          smart-comment-mark-string))

(defun smart-comment-cleanup ()
  "Remove lines marked for deletion"
  (interactive "*")
  (let ((search (smart-comment-mark)))
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
    (setq comment-start (smart-comment-mark))
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
  (cond ((= 16 (prefix-numeric-value arg))
         (smart-comment-cleanup))
        ((and mark-active transient-mark-mode)
         (smart-comment-region (region-beginning) (region-end) arg))
        ((looking-at "\\s-*$")
         (funcall smart-comment-eol-action arg))
        ((smart-comment-is-at-beg)
         (funcall smart-comment-beg-action arg))
        (t (funcall smart-comment-mid-action arg))))

(provide 'smart-comment)
