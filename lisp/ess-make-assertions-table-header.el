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
        (n-cols-diff)
        (nchar-padding 2)
        (print-row))

    ;; conditionally remove the last field columns if the assertions don't have
    ;; both arguments
    (setq n-cols-diff (- (length field-entries-1) (length col-positions)))
    (setq field-entries-1 (butlast field-entries-1 n-cols-diff))
    (setq field-entries-2 (butlast field-entries-2 n-cols-diff))

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
      (scan-error "reached the end of the line without finding function or remaining arguments")))

  (defun throw-if-diff-lines (a b)
    (if (eq (line-number-at-pos a) (line-number-at-pos b))
        b
      (error "the assertion statement is not all on the same line")))

  (let ((positions '())
        (col-positions '())
        (is-sep-found))

    ;; move to the first non-whitespace character on the current line if the
    ;; line is nonempty, or the next non-whitespace character otherwise
    (move-to-column 0)
    (skip-chars-forward "[:space:]")
    (throw-if-eol)
    (setq positions (cons (point) positions))
    (setq col-positions (cons (current-column) col-positions))

    ;; find beginning of first argument
    (skip-chars-forward "^(")
    (forward-char)
    (skip-chars-forward "[:space:]")
    (throw-if-eol)
    (setq positions (cons (point) positions))
    (setq col-positions (cons (current-column) col-positions))

    ;; try to find the beginning of the second argument and use `is-sep-found'
    ;; to signal whether or not there is a second argument
    (condition-case nil
        (progn
          (dp-r-find-next-fcn-arg-separator)
          (setq is-sep-found t))
      (scan-error (setq is-sep-found nil)))

    ;; if we've found a function formal arguments separator (i.e. a comma), then
    ;; find the start of the next argument and update `positions` and
    ;; 'col-positions'
    (when is-sep-found
      (forward-char)
      (skip-chars-forward "[:space:]")
      (throw-if-eol)
      (setq positions (cons (point) positions))
      (setq col-positions (cons (current-column) col-positions)))

    ;; ensure that all of the positions are on the same line
    (seq-reduce #'throw-if-diff-lines (cdr positions) (car positions))

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
  ;; Note that `forward-sexp' throws a `scan-error' signal if we reach the
  ;; closing parenthesis of the function argument list, as desired.  The call to
  ;; `eobp' is to protect against an infinite loop in the event of a malformed
  ;; function or if the function is called outside of a function argument list.
  (while (not (eq (char-after) ?,))
    (forward-sexp)
    (skip-chars-forward "[:space:]\n")
    (when (eobp)
      (signal search-failed "reached buffer end without finding the function end")))

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
