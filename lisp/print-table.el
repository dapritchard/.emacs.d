;; -*- lexical-binding: t -*-


(defun dp-print-row (field-entries col-positions nchar-padding)
  "Print a row of a table.

Inserts a line into the buffer after the line in which point
resides.  FIELD-ENTRIES and COL-POSITIONS are each expected to be
lists of the same length, while NCHAR-PADDING is a nonnegative
integer.  Furthermore, the elements of FIELD-ENTRIES are all
expected to be strings, while the elements of COL-POSITIONS are
all expected to be nonnegative integers of increasing value.  The
form of the input data is not checked.

Each element of FIELD-ENTRIES is inserted into the line at the
column specified by the corresponding element in COL-POSITIONS.
If the previous entry is too long to fit in before the current
entry with NCHAR-PADDING amount of spaces, then the previous
entry is truncated."

  ;; tracks whether the next column to insert is the first column
  (let ((is-first-col t))

    ;; each iteration inserts the next field into the current line
    (while field-entries
      (let* ((curr-field (car field-entries))
             (curr-col (car col-positions))
             (nchar-betw-cols (- curr-col (current-column))))

        ;; conditionally delete characters if this we are past the start of the
        ;; current column minus the required padding size (although we ignore this
        ;; rule for the first column)
        (when (and (not is-first-col) (< nchar-betw-cols nchar-padding))
          (delete-char (- nchar-betw-cols nchar-padding)))

        ;; insert spaces until the start of the current column and then insert
        ;; the field
        (insert-char ?\s (- curr-col (current-column)))
        (insert curr-field)

        ;; advance the field value and column location lists, and signify that we
        ;; are past the first column
        (setq field-entries (cdr field-entries))
        (setq col-positions (cdr col-positions))
        (setq is-first-col nil)))

    (newline)))




(defun dp-make-print-row (col-positions nchar-padding)
  "Create a closure for dp-print-row.

Returns a function that takes a single argument FIELD-ENTRIES.
This argument is passed on to `dp-print-row' along with the
entries for COL-POSITIONS and NCHAR-PADDING.

The validity of the form of COL-POSITIONS and NCHAR-PADDING is
checked at the time the function is called, while the validity of
the input to the returned function is checked at the time this
function is called."
  (let ((start-col-positions col-positions)
        (len-col-positions)
        (prev-col-pos))

    ;; check that NCHAR-PADDING is a nonnegative integer and that COL-POSITIONS
    ;; is a list of all integers
    (assert (and (integerp nchar-padding) (<= 0 nchar-padding)))
    (assert (listp col-positions))
    (assert (not (member nil (mapcar #'integerp col-positions))))

    ;; get the number of columns
    (setq len-col-positions (length col-positions))

    ;; ensure that the first column position is nonnegative, and advance
    ;; COL-POSITIONS by one element
    (when (<= 1 len-col-positions)
      (setq prev-col-pos (car col-positions))
      (assert (<= 0 prev-col-pos))
      (setq col-positions (cdr col-positions)))

    ;; ensure that each column position is at least NCHAR-PADDING positions
    ;; greater than the previous position
    (while col-positions
      (assert (<= prev-col-pos (- (car col-positions) nchar-padding)))
      (setq prev-col-pos (car col-positions))
      (setq col-positions (cdr col-positions)))

    (lambda (field-entries)
      (assert (eq (length field-entries) len-col-positions))
      (assert (not (member nil (mapcar #'stringp field-entries))))
      (dp-print-row field-entries start-col-positions nchar-padding)
      )))
