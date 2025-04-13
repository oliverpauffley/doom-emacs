;;; lisp/major-mode.el -*- lexical-binding: t; -*-

(require 'derived)

(defvar quip-mode-hook nil
  "*List of functions to call when entering Quip mode.")

(defalias 'backward-quip `backward-page)
(defalias 'forward-quip `forward-page)
(defalias 'narrow-to-quip `narrow-to-page)
(defalias 'what-quip `what-page)


(defun count-quips ()
  "Count the quips in the buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (count-matches "^%%$"))))

(define-derived-mode quip-mode text-mode "Quip"
                     "Major mode for editing Quip files.
   Special commands:
\\{quip-mode-map}"
                     ;; set local variables
                     (make-local-variable 'paragraph-start)
                     (make-local-variable 'paragraph-separate)
                     (make-local-variable 'page-delimiter)
                     (setq paragraph-start "%%\\|[ \t\n\^L]")
                     (setq paragraph-separate "%%$\\|[ \t\n\^L]*$")
                     (setq page-delimiter "^%%$"))

(define-key quip-mode-map "\C-x[" 'backward-quip)
(define-key quip-mode-map "\C-x]" 'forward-quip)
(define-key quip-mode-map "\C-xnq" 'narrow-to-quip)
(define-key quip-mode-map "\C-cw" 'what-quip)

(provide 'quip)
