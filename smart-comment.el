;;; smart-comment.el --- smarter commenting -*- lexical-binding: t; -*-

;; Smart comment

;;* Customization
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

(defun smart-comment-is-at-beg ()
  (let ((orig-point (point)))
    (save-excursion (back-to-indentation) (<= orig-point (point)))))

;;;###autoload
(defun smart-comment-cleanup ()
  "Remove lines marked for deletion"
  (interactive "*")
  (let ((search (regexp-quote (smart-comment-mark))))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward search nil t)
        (kill-whole-line)))))

;;;###autoload
(defun smart-comment-mark-line (arg)
  "Mark a line for deletion"
  (interactive "*P")
  (smart-comment-line 4))

;;;###autoload
(defun smart-comment-mark-region (beg end arg)
  "Mark a region for deletion"
  (interactive "*r\nP")
  (save-excursion
    (goto-char beg) (skip-chars-forward " \t\n\r")
    (while (< (point) end)
     (insert smart-comment-mark-string " ")
     (forward-line)
     (back-to-indentation)
     (setq end (+ end (length smart-comment-mark-string) 1))))
  (comment-or-uncomment-region beg end))

;;;###autoload
(defun smart-comment-region (beg end arg)
  "Comment or uncomment a region"
  (interactive "*r\nP")
  (if (= 4 (prefix-numeric-value arg)) (smart-comment-mark-region beg end arg)
    (comment-normalize-vars)
    (if (not (comment-only-p beg end))
        (comment-region beg end arg)
      (let ((search (regexp-quote (smart-comment-mark)))
            (len (+ 1 (length smart-comment-mark-string))))
        (save-excursion
          (goto-char beg)
          (while (re-search-forward search end t)
            (delete-backward-char len)
            (setq end (- end len))))
        (uncomment-region beg end arg)))))

;;;###autoload
(defun smart-comment-line (arg)
  "Comment or uncomment a line"
  (interactive "*P")
  (smart-comment-region (line-beginning-position) (line-end-position) arg))

;;;###autoload
(defun smart-comment (arg)
  "Smart commenting"
  (interactive "*P")
  (cond ((= 16 (prefix-numeric-value arg))
         (smart-comment-cleanup))
        ((and mark-active transient-mark-mode)
         (smart-comment-region (region-beginning) (region-end) arg))
        ((looking-at "\\s-*$")
         (funcall smart-comment-end-action arg))
        ((smart-comment-is-at-beg)
         (funcall smart-comment-beg-action arg))
        (t (funcall smart-comment-mid-action arg))))

(provide 'smart-comment)
