(defun dp-info (&optional file-or-node)
  "Open an info buffer with a unique filename.

This function opens an info page as specified by FILE-OR-NODE.
The name of the newly created buffer is of the form
\"*info*<N>\", where N is taken to be one more than the next
largest value of N for a buffer of this form.  If there are no
buffers yet of this form, then N starts at 1.

See the documentation for `info' for a description of the
semantics of FILE-OR-NODE."
  (interactive)
  (defun is-info-p (string)
    (string-match-p "\\*info\\*<[[:digit:]]+>" string))
  (defun extract-info-num-string (string)
    (replace-regexp-in-string "\\*info\\*<\\([[:digit:]]+\\)+>" "\\1" string))
  (defun max-of-num-list (num-list)
    (if num-list
        (car (sort num-list #'>))
      0))
  (defun num-to-name (n))
  (let* ((buf-nm-list              (mapcar #'buffer-name (buffer-list)))
         (buf-info-nm-list         (seq-filter #'is-info-p buf-nm-list))
         (buf-info-num-string-list (mapcar #'extract-info-num-string buf-info-nm-list))
         (buf-info-num-list        (mapcar #'string-to-number buf-info-num-string-list))
         (buf-info-max-plus1-num   (1+ (max-of-num-list buf-info-num-list)))
         (new-buf-nm               (concat "*info*<" (number-to-string buf-info-max-plus1-num) ">")))
    (info file-or-node new-buf-nm)))


(defun dp-info-dired ()
  "Open the Dired node in the Emacs info."
  (interactive)
  (dp-info "(Emacs) Dired"))


(defun dp-info-dired-x ()
  "Open the Dired-x info."
  (interactive)
  (dp-info "(Dired-x)"))



(defun dp-info-elisp ()
  "Open the Emacs Lisp info."
  (interactive)
  (dp-info "(elisp)"))


(defun dp-info-emacs ()
  "Open the Emacs info."
  (interactive)
  (dp-info "(Emacs)"))


(defun dp-info-sicp ()
  "Open the SICP info."
  (interactive)
  (dp-info "~/.emacs.d/info/sicp.info.gz"))
