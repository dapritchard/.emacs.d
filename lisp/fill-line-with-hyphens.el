(defun dp-fill-line-with-hyphens ()
  "Fills the current line with hyphens up to the fill column.

If the current line is longer than the fill column, then remove
hyphens until either the fill column is reached, or we find a
non-hyphen character, whichever comes first."
  (interactive)

  ;; delete any whitespace at the end of the line
  (end-of-line)
  (delete-horizontal-space)

  ;; create helper variables.  The tasks that the function need to do are
  ;; mainly dependent on whether we are before or after the fill column, and
  ;; whether there are already trailing columns or not.
  (let* ((is-last-char-hyphen (char-equal ?- (preceding-char)))
	 (n-char-before-fill-column (- fill-column (current-column))))
    (cond
     ;; case: there's already some hyphens but not enough
     ((and (> n-char-before-fill-column 0) is-last-char-hyphen)
      (insert-char ?- n-char-before-fill-column))
     ;; case: there's no hyphens and we have room to add at least one space and
     ;; one hyphen
     ((> n-char-before-fill-column 1)
      (progn
        (insert-char ?\s)
        (insert-char ?- (1- n-char-before-fill-column))))
     ;; case: there's hyphens after the fill column that need to be removed
     ((and (< n-char-before-fill-column 0) is-last-char-hyphen)
      (progn
	(re-search-backward "[^-]")
	(forward-char)
	(kill-line)
	(dp-fill-line-with-hyphens))))))
