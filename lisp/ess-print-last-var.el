(defun dp-r-last-var-transf (prefix suffix)
  "Concatenate PREFIX, the last assigned variable, and SUFFIX."
  (interactive "sprefix text:  \nssuffix text:  ")
  (let ((var-name))
    (dp-r-find-last-command)
    (setq var-name (dp-r-find-var-assign-after-point))
    (goto-char (point-max))
    (insert prefix var-name suffix)))


(defun dp-r-last-var-plain ()
  "Insert \"var\", where var is the last assigned variable."
  (interactive)
  (dp-r-last-var-transf "" ""))


(defun dp-r-last-var-print ()
  "Insert \"print(var)\", where var is the last assigned variable."
  (interactive)
  (dp-r-last-var-transf "print(" ")"))


(defun dp-r-last-var-str ()
  "Insert \"str(var)\", where var is the last assigned variable."
  (interactive)
  (dp-r-last-var-transf "str(" ")"))


(defun dp-r-find-last-command ()
  "Move point to the beginning of the last command.

TODO: the ess-switch-to-inferior-or-script-buffer command
requires a follow-up keypress to complete.  Figure out how to do
this in elisp."
  (let ((end-of-line-position))
    (when (not (derived-mode-p 'inferior-ess-r-mode))
      (ess-switch-to-inferior-or-script-buffer nil))
    (goto-char (point-max))
    (move-beginning-of-line nil)
    (re-search-backward "^[:blank:]*>")

    ;; move point past any additional ">" prompts
    (setq end-of-line-position (save-excursion (move-end-of-line nil)
                                               (point)))
    (while (re-search-forward "[:blank:]*>" end-of-line-position t))
    ;; move point to the start of the next word
    (skip-chars-forward "[[:blank:]]" end-of-line-position)
    ))


(defun dp-r-find-var-assign-after-point ()
  "Obtain the variable name assigned a value on the current line.

TODO: this is a very rough implementation."
  (let ((start-pos (point))
        (after-pos))
    (re-search-forward "[[:blank:]]\\|$")
    (skip-chars-backward "[[:blank:]]" start-pos)
    (setq after-pos (point))
    (buffer-substring-no-properties start-pos after-pos)))
