(defun dp-ess-find-last-command ()

  (interactive)  ;; TODO: make non-interactive

  (let ((end-of-line-position))
    (goto-char (point-max))
    (move-beginning-of-line nil)
    (re-search-backward "^[:blank:]*>")

    ;; move point past any additional ">" prompts
    (setq end-of-line-position (save-excursion (move-end-of-line nil)
                                               (point)))
    (while (re-search-forward "[:blank:]*>" end-of-line-position t))
    ;; move point to the start of the next word
    (re-search-forward "[^[:blank:]]" end-of-line-position)
    (backward-char)))


(defun dp-ess-find-var-assign-after-point ()
  "Obtain the variable name assigned a value on the current line.

TODO: this is a very rough implementation."
  (interactive)

  (let ((start-pos (point))
        (after-pos))

    (re-search-forward "[[:blank:]]")
    (setq after-pos (point))
    (message "%s" (buffer-substring-no-properties start-pos after-pos))
    (buffer-substring-no-properties start-pos after-pos)))


(defun testme ()
  (interactive)
  (dp-ess-find-last-command)
  (dp-ess-find-var-assign-after-point))
