;;; mmark-test.el ---- Tests for mmark. -*- lexical-binding: t -*-
;;; Commentary:
;; Suite of simple tests to verify the package works as expected.

;;; Code:

(require 'ert)
;; We want to test this file, not one that could be in the path.
(load-file "./mmark.el")
(require 'mmark)


(defvar mmark--testbuf "mmark-test-buffer")

(defun clean-mmask-hashmap (body)
  "Clean the mmark hashmap, then apply BODY."
  (unwind-protect
      (progn
        ;; Set up
        (dolist
            (k (map-keys mmark--hashmap))
          (map-delete mmark--hashmap k)
          )
        ;; Function call
        (funcall body)
        )
    ;; Tear down
    )
  )

(defun create-clean-buffer (buf body)
  "Run BODY in BUF."
  (unwind-protect
      (progn
        ;; Set up
        (generate-new-buffer buf)
        (with-current-buffer buf
          (insert "some text to write\nand 11 some more")
          ;; Function call
          (funcall body)
          )
        )
    ;; Tear down
    (kill-buffer buf)
    )
  )

(defun get-text (begin end)
  "In the test buffer, select the text from BEGIN to END."
  (create-clean-buffer mmark--testbuf
   (lambda ()
     (progn
       (goto-char begin)
       (push-mark)
       (forward-char end)
       (kill-ring-save nil nil t)
       (current-kill 0)
       )
     )
   )
  )

(defun set-mmark-pos (pos chr)
  "Mark using CHR at POS in the test buffer."
  (create-clean-buffer mmark--testbuf
   (lambda ()
     (progn
       (goto-char pos)
       (mmark-set chr)
       )
     )
   )
  )

(defun get-mmark-hash-entry (chr)
  "Retrieve the object stored in the map for the test buffer and CHR."
  (let (
        (key (mmark--key-helper mmark--testbuf chr))
        )
    (map-elt mmark--hashmap key)
    )
  )

(ert-deftest test-fixture ()
  (should
   (string=
    (get-text 0 4)
    "some"
    )
   )
  )

(ert-deftest test-set-mark ()
  "Check the mark is correctly set in the hashmap."
  (should
   (=
    (clean-mmask-hashmap
     (lambda ()
       (progn
         (set-mmark-pos 4 ?a)
         (mmark--mark-cls-point (get-mmark-hash-entry ?a))
         )
       )
     )
    4
    )
   )
  )

(ert-deftest test-get-mark-unset ()
  "Get nil when trying to get an unset mark."
  (should
   (eql
    (clean-mmask-hashmap
     (lambda ()
       (progn
         (get-mmark-hash-entry ?a)
         )
       )
     )
    nil
    )
   )
  )


;;; mmark-test.el ends here
