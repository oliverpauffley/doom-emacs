;;; lisp/sloc.el -*- lexical-binding: t; -*-

(defun count-sloc--act-on-region-or-buffer (func message)
  "Perform FUNC on region or buffer and print MESSAGE.
FUNC should accept two arguments, the beginning and end of the range
it operates on. If MESSAGE contains a placeholder (e.g. \"%s\"), the return value
of FUNC is substituted for it."
  (let (begin end)
    (if (use-region-p)
        (setq begin (region-beginning) end (region-end))
      (setq begin (point-min) end (point-max)))
    (message message (funcall func begin end))))

(defun count-sloc--count-lines-if (predicate begin end)
  "Count lines satisfying PREDICATE from BEGIN to END.
PREDICATE should accept no arguments."
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char begin)
      (let ((count 0))
        (while (not (eobp))
          (when (funcall predicate)
            (setq count (1+ count)))
          (forward-line))
        count))))

(defun count-sloc--comment-or-blank-line-p ()
  "Return t if the point is at a comment line.
Assume that the point is at the beginning of line."
  (unless (nth 3 (syntax-ppss))
    (save-excursion
      (let ((orig-line-number (line-number-at-pos)))
        (forward-comment 1000)
        (or (not (eq orig-line-number (line-number-at-pos)))
            (eobp))))))

(defun count-sloc--negate (fun)
  "Return a function returning the logic opposite of FUN."
  (lambda (&rest args)
    (not (apply fun args))))

(defun count-sloc ()
  "Count non-blank lines in the region or buffer"
  (interactive)
  (count-sloc--act-on-region-or-buffer
   (apply-partially #'count-sloc--count-lines-if
                    (count-sloc--negate #'count-sloc--comment-or-blank-line-p))
   "Non-blank lines: %s"))
