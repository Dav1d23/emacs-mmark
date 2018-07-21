;;; mmark.el ---- An easy (and bugged) replacement for vim marks.
;;; Commentary:
;; This mini-package is a pretty fast attempt to have the same mark mechanism
;; of vim.
;; mark-set will get a char and use it to set the position in the current buffer.
;; mark-get will do the opposite.

;; TODO:
;; - remove the FIXME below.

;;; Code:

(require 'map)
(defvar mmark--hashmap
  (make-hash-table)
  "This variable will hold the hashes.")

(defun mmark--prehash-string (mstr)
  "Prehash MSTR in order to have a number at the end."
  ;; FIXME this version will have a flawless: permutation of
  ;; the same buffer name have the same pre-hash of another file.
  ;; Example: to_to_99 and 9_otto_9 will have the same counter, thus the same _hash_ :'(.
  (let (charlist mvar (counter 0))
    (setq charlist (string-to-list mstr))
    (while charlist
      (setq mvar (car charlist))
      (setq counter (+ counter mvar))
      (setq charlist (cdr charlist))
      )
    counter
    )
  )

(defun mmark-set (char)
  "CHAR is the value I must put in the map."
  (interactive "*cChar:")
  (let (mstr)
    (setq mstr
	  (mmark--prehash-string (format "%s_%s" (buffer-name) char))
	  )
    ;; Override with the same key
    (if (map-contains-key mmark--hashmap mstr)
	(map-delete mmark--hashmap mstr)
      )
    (map-put mmark--hashmap mstr (point))
    (message "Set mark `%c` for buffer `%s`." char (buffer-name))
    )
  )

(defun mmark-get (char)
  "CHAR is the value I must retrieve from the map."
  (interactive "*cChar:")
  (let (mstr)
    (setq mstr
	  (mmark--prehash-string (format "%s_%s" (buffer-name) char))
	  )
    ;; Get the key, if any
    (if (map-contains-key mmark--hashmap mstr)
	(goto-char (map-elt mmark--hashmap mstr))
      (message "Unable to find mark `%c` in buffer `%s`"
	       char
	       (buffer-name))
      )
    )
  )

(local-set-key (kbd "C-m s") 'mmark-set)
(local-set-key (kbd "C-m g") 'mmark-get)

(provide 'mmark)

;;; mmark.el ends here
