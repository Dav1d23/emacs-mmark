;;; mmark.el ---- A simple replacement for vim marks. -*- lexical-binding: t -*-
;;; Commentary:
;; This mini-package is a pretty fast attempt to have the same mark mechanism
;; of vim.
;; mmark-set gets a char and uses it to set the position in the current buffer.
;; mmark-get does the opposite, jumping to the defined tag.
;; mmark-mode enters tabulate-mode, where you can select an entry and jump to the
;;            appropriate buffer.
;;
;; In mmark-mode the following keymap is defined:
;;     g    -> refresh the entries
;;     d    -> remove an entry
;; <return> -> go to the entry highlighted with the cursor

;; TODO:
;; - get the 3 surrounding lines of the mark, to easy spot what it is about.
;; - what to do with buffer that are changed and/or removed?


;;; Code:

(require 'cl-extra)
(require 'cl-lib)
(require 'cl-macs)
(require 'map)
(require 'tabulated-list)


(defvar mmark--hashmap
  (make-hash-table)
  "This variable will hold the marks.")


(cl-defstruct mmark--mark-cls
  "The data structure containing data to display."
  buffer point char)


(defun mmark--key-helper (buf char)
  "Get the key hash based on BUF and CHAR."
  (sxhash-equal (format "%s_%s" buf char))
  )


(defun mmark-set (char)
  "CHAR is the value I must put in the map."
  (interactive "*cChar:")
  (let* (
         (buf (buffer-name))
         (map_entry (make-mmark--mark-cls :buffer buf :point (point) :char char))
         (key (mmark--key-helper buf char))
         )
    (setf (map-elt mmark--hashmap key) map_entry)
    (message "Set mark `%c` for `%s`" char (buffer-name))))


(defun mmark-get (char)
  "CHAR is the value I must retrieve from the map."
  (interactive "*cChar:")
  (let* (
         (buf (buffer-name))
         (key (mmark--key-helper buf char))
         )
    ;; Get the key, if any
    (if (map-contains-key mmark--hashmap key)
	(goto-char (mmark--mark-cls-point (map-elt mmark--hashmap key)))
      (message "Unable to find mark `%c` in `%s`." char buf))))


(defun mmark--create-entries ()
  "Create the entries for 'tabulated-list-entries'."
  (let (
        (map-to-list (map-pairs mmark--hashmap))
        (entries '())
        )

    (dolist (el map-to-list entries)
      (let* (
             (val (cdr el))
             (key (format "%s" (car el)))
             (buf (format "%s" (mmark--mark-cls-buffer val)))
             (chr (format "%c" (mmark--mark-cls-char val)))
             (pnt (format "%s" (mmark--mark-cls-point val)))
             )
        (push (list key (vector buf chr pnt)) entries)
        )
      )
    entries
    )
  )


(define-derived-mode mmark-mode tabulated-list-mode "mmark"
  "mmark-mode definition."
  (let (
        (entries (mmark--create-entries))
        )

    (setq tabulated-list-format [("Buffer" 30 t)
                                 ("Char" 5 nil)
                                 ("Point" 0 nil)
                                 ])

    (setq tabulated-list-entries entries)
    (setq tabulated-list-padding 4)
    (setq tabulated-list-sort-key (cons "Buffer" nil))
    (tabulated-list-init-header)
    (tabulated-list-print t)))


(defun mmark--jump-buffer ()
  "Jump to the buffer highlighted in the mmark-mode."
  (interactive)
  (let* (
         (key (cl-parse-integer (tabulated-list-get-id)))
         (val (map-elt mmark--hashmap key))
         (buf (mmark--mark-cls-buffer val))
         (pnt (mmark--mark-cls-point val))
         )
    (switch-to-buffer-other-window buf)
    (goto-char pnt)
    (message "jumped to %s [%s]" buf pnt)
    ))


(defun mmark--remove-entry ()
  "Remove the entry from the hashmap."
  (interactive)
  (let* (
         (key (cl-parse-integer (tabulated-list-get-id)))
         )
    (setq mmark--hashmap (map-delete mmark--hashmap key))
    (message "Removed %s" key)
    (mmark-mode)
    ))


(defun mmark ()
  "Run mmark-mode in *mmark* buffer."
  (interactive)
  (switch-to-buffer "*mmark*")

  ;; Remove the underlying entry.
  (define-key mmark-mode-map (kbd "g") 'mmark-mode)
  ;; Remove the underlying entry.
  (define-key mmark-mode-map (kbd "d") 'mmark--remove-entry)
  ;; Gets you to the right location.
  (define-key mmark-mode-map (kbd "<return>") 'mmark--jump-buffer)

  (mmark-mode)
  )


(provide 'mmark)

;;; mmark.el ends here
