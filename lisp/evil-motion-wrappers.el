;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

(defun dp-evil-normal-state-then-ace-window (arg)
  "Wrapper for `ace-window' that first changes the evil state to normal state."
  (interactive "p")
  (evil-normal-state t)
  (ace-window arg))


(defun dp-evil-normal-state-then-avy-goto-char-2 (char1 char2 &optional arg beg end)
  "Wrapper for `avy-goto-char-2' that first changes the evil state to normal state."
  (interactive (list (read-char "char 1: " t)
                     (read-char "char 2: " t)
                     current-prefix-arg
                     nil nil))
  (evil-normal-state t)
  (avy-goto-char-2 char1 char2 arg beg end))
