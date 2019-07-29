;; -*- lexical-binding: t -*-

(defun dp-ess-word-at-point-to-string ()
  "Find the word at point and return it as a string."
  (save-excursion
    (buffer-substring
     (+ (point) (skip-chars-backward "a-zA-Z0-9._"))
     (+ (point) (skip-chars-forward "a-zA-Z0-9._")))))


(defun dp-ess-eval-word ()
  "Evaluate the word at point in the inferior R process."
  (interactive)
  (ess-eval-linewise (dp-ess-word-at-point-to-string)))
