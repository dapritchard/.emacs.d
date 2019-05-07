;; -*- lexical-binding: t -*-


;; test dp-r-insert-testthat-table-header --------------------------------------

(let ((testdata-header-table-1 "# assertion      actual         target\n")
      (testdata-header-table-2 "# ---------      ------         ------\n")
      (testdata-all-one-line   "expect_identical(my_function(), \"my_result\")"))

  (ert-deftest dp-r-insert-testthat-table-header/normal-operations ()
    (with-temp-buffer
      (insert testdata-all-one-line)
      (dp-r-insert-testthat-table-header)
      (should (equal (buffer-string)
                     (concat testdata-header-table-1
                             testdata-header-table-2
                             testdata-all-one-line))))))
