;;; mmark-test.el ---- Tests for mmark. -*- lexical-binding: t -*-
;;; Commentary:
;; Suite of simple tests to verify the package works as expected.

;;; Code:

(require 'ert)

;; We want to test this file, not one that could be in the path.
(load-file "./mmark.el")
(require 'mmark)


(ert-deftest
 test-set-mark ()
 "Check the mark is correctly set in the hashmap."
 (should
  (=
   (with-temp-buffer
     (insert "some text to write\nand 11 some more")
     (goto-char 12)
     (mmark-set ?a)
     (mmark-get ?a)
     )
   12
   )
  )
 )

(ert-deftest
 test-get-mark-unset ()
 "Get nil when trying to get an unset mark."
 (should
  (string-prefix-p "Unable to find mark ‘z‘ in"
   (with-temp-buffer
     (insert "some text to write\nand 11 some more")
     (goto-char 4)
     (mmark-set ?a)
     (mmark-get ?z)
     )
   )
  )
 )


;;; mmark-test.el ends here
