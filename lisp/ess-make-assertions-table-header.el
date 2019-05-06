;; -*- lexical-binding: t -*-


(defun dp-r-insert-testthat-table-header ()
  (let ((starting-point (point))
        (starting-line (line-number-at-pos))
        (separator-positions)
        (column-positions)
        (req-nchar-colsep 2))

    ;; find the table header column positions
    (forward-line)
    (if ((eq (line-number-at-pos) starting-line))
        (error "no following line"))
    (setq separator-positions (dp-r-find-testthat-table-positions))
    (setq column-positions (mapcar current-column buffer-positions))

    ;; insert newline
    (end-of-line 0)
    (newline)

    ;; insert assertion header
    (move-to-column (car column-positions))
    (insert "# assertion")

    ;; conditionally delete characters when the length of the column header is
    ;; longer than the start of the next column minus the required padding size,
    ;; and insert spaces until the start of the next column
    (let ((nchar-betw-cols (- (cadr column-positions) (current-column))))
      (when (< nchar-betw-cols req-nchar-colsep)
        ((delete-char (- nchar-betw-cols req-nchar-colsep)))))
    (insert-char ?\s (- (cadr column-positions) (current-column)))


    ()
    ))







;; (defun dp-make-field-entry-list (entries positions)
;;   ;; ENTRIES and POSITIONS must be lists of the same length, with the elements
;;   ;; of ENTRIES all strings, and the elements of POSITIONS all nonnegative
;;   ;; integers that are increasing in value
;;   (assert (and (listp entries) (listp positions) (eq (length entries) (length positions))))
;;   (assert (not (member nil (mapcar #'stringp entries))))
;;   (assert (not (member nil (mapcar #'integerp positions))))
;;   )

;; (defun dp-r-insert-row (field-entry-list req-nchar-colsep)


;;   ;; insert newline
;;   (end-of-line 0)
;;   (newline)

;;   (while field-entry-list
;;     (let ((entry-val (car (assoc 'entries field-entry-list)))
;;           (col-pos (car (assoc 'col-positions field-entry-list))))

;;       ;; conditionally delete characters if we are past the start of the next
;;       ;; column minus the required padding size
;;       (let ((nchar-betw-cols (- col-pos (current-column))))
;;         (when (< nchar-betw-cols req-nchar-colsep)
;;           ((delete-char (- nchar-betw-cols req-nchar-colsep)))))

;;       ;; insert spaces until the start of the next column and insert field
;;       (insert-char ?\s (- col-pos (current-column)))
;;       (insert entry-val)

;;       (setq field-entry-list (cdr field-entry-list))
;;       ))
;;   )


(defun dp-r-find-testthat-table-positions ()
  (interactive)
  (let ((assertion-pos)
        (actual-pos)
        (target-pos))

    ;; find beginning of line
    (back-to-indentation)
    (setq assertion-pos (point))

    ;; find beginning of first argument
    (skip-chars-forward "^(")
    (forward-char)
    (skip-chars-forward "[:space:]")
    (setq actual-pos (point))

    ;; find beginning of second argument
    (dp-r-find-next-fcn-arg-separator)
    (forward-char)
    (skip-chars-forward "[:space:]")
    (setq target-pos (point))

    (list assertion-pos actual-pos target-pos)))




(defun dp-r-find-next-fcn-arg-separator ()
  "Find the next argument separator in a function call.

Move point to the next function argument separator.  Point is
expected to be at the opening parenthesis of the function
argument list or on one of the separators of the function
argument list.

Throws an error if there are no more top-level function
separators."
  (interactive)

  ;; move past the current character and any whitespace
  (forward-char)
  (skip-chars-forward "[:space:]")

  ;; each iteration moves point across one sexp and any trailing whitespace.
  ;; Note that `forward-sexp' throws an error if we reach the closing
  ;; parenthesis of the function argument list, as desired.  The call to `eobp'
  ;; is to protect against an infinite loop in the event of a malformed function
  ;; or if the function is called outside of a function argument list.
  (while (not (eq (char-after) ?,))
    (forward-sexp)
    (skip-chars-forward "[:space:]")
    (when (eobp)
      (error "reached buffer end without finding the function end")))

  ;; if we've made it here then an argument separator was found
  (point))




(defun dp-r-find-fcn-arg-separators ()
  "Find the function argument separator positions.

Returns a list of the function argument separator positions.
Assumes that point is at the opening parenthesis of the function
argument list."
  (interactive)

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

    ;; reverse the positions list and return
    (reverse sep-positions)))
