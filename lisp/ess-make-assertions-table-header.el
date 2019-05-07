;; -*- lexical-binding: t -*-


(defun dp-r-insert-testthat-table-header ()
  "Insert a testthat table header.

Inserts two lines of commented code above a testthat assertion
function aligning with the assertion arguments.  Assumes that
point is on a line with a testthat assertion function, and that
the first two arguments corresponding to the actual value and the
target value are on the same line."
  (interactive)
  (let ((field-entries-1 '("# assertion" "actual" "target"))
        (field-entries-2 '("# ---------" "------" "------"))
        (col-positions (dp-r-find-testthat-table-positions))
        (nchar-padding 2)
        (print-row))

    ;; insert assertion header
    (move-to-column 0)
    (setq print-row (dp-make-print-row col-positions nchar-padding))
    (funcall print-row field-entries-1)
    (funcall print-row field-entries-2)))




(defun dp-r-find-testthat-table-positions ()
  "Find the column positions for a testthat table header.

Assumes that point is on a line with a function call and at least
two arguments.  Returns the column position of the first
non-whitespace character on the line, the start of the first
argument of the function call, and the start of the second
argument of the function call."

  (defun throw-if-eol ()
    (when (eolp)
      (error "reached the end of the line without finding two function arguments")))

  (let ((assertion-pos)
        (actual-pos)
        (target-pos)
        (col-positions '()))

    ;; move to the first non-whitespace character on the current line if the
    ;; line is nonempty, or the next non-whitespace character otherwise
    (move-to-column 0)
    (skip-chars-forward "[:space:]")
    (throw-if-eol)
    (setq assertion-pos (point))
    (setq col-positions (cons (current-column) col-positions))

    ;; find beginning of first argument
    (skip-chars-forward "^(")
    (forward-char)
    (skip-chars-forward "[:space:]")
    (throw-if-eol)
    (setq actual-pos (point))
    (setq col-positions (cons (current-column) col-positions))

    ;; find beginning of second argument
    (dp-r-find-next-fcn-arg-separator)
    (forward-char)
    (skip-chars-forward "[:space:]")
    (throw-if-eol)
    (setq target-pos (point))
    (setq col-positions (cons (current-column) col-positions))

    ;; ensure that all three positions are on the same line
    (unless (and (eq (line-number-at-pos assertion-pos) (line-number-at-pos actual-pos))
                 (eq (line-number-at-pos actual-pos) (line-number-at-pos target-pos)))
      (error "the assertion statement is not all on the same line"))

    (reverse col-positions)))




(defun dp-r-find-next-fcn-arg-separator ()
  "Find the next argument separator in a function call.

Move point to the next function argument separator.  Point is
expected to be at the opening parenthesis of the function
argument list or on one of the separators of the function
argument list.

Throws an error if there are no more top-level function
separators."

  ;; move past the current character and any whitespace
  (if (eolp)
      (beginning-of-line 2)
    (forward-char))
  (skip-chars-forward "[:space:]\n")

  ;; each iteration moves point across one sexp and any trailing whitespace.
  ;; Note that `forward-sexp' throws an error if we reach the closing
  ;; parenthesis of the function argument list, as desired.  The call to `eobp'
  ;; is to protect against an infinite loop in the event of a malformed function
  ;; or if the function is called outside of a function argument list.
  (while (not (eq (char-after) ?,))
    (forward-sexp)
    (skip-chars-forward "[:space:]\n")
    (when (eobp)
      (error "reached buffer end without finding the function end")))

  ;; if we've made it here then an argument separator was found
  (point))




(defun dp-r-find-fcn-arg-separators ()
  "Find the function argument separator positions.

Returns a list of the function argument separator positions.
Assumes that point is at the opening parenthesis of the function
argument list."

  ;; find the position of the next function argument separator if any
  ;; separaotors remain, or return nil otherwise
  (defun try-find-next ()
    (ignore-errors (dp-r-find-next-fcn-arg-separator)))

  ;; SEP-POSITIONS is used to track all of the found function argument separator
  ;; positions, while CURR-POSITIONS is bound to the next separator position
  (let ((sep-positions)
        (curr-pos (try-find-next)))

    ;; each iteration adds the most recently found function argument separator
    ;; position to SEP-POSITIONS, and searches for another separator position
    (while curr-pos
      (setq sep-positions (cons curr-pos sep-positions))
      (setq curr-pos (try-find-next)))

    (reverse sep-positions)))
