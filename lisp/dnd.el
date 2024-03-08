;;; lisp/dnd.el -*- lexical-binding: t; -*-

(defun insert-dnd-org-table ()
  "Inserts a new stat block for a d&d character at point."
  (interactive "*")
  (insert "|Name|AC|Hit Points| Speed|CR||")
  (forward-line 1)
  (org-table-hline-and-move)
  (forward-line 1)
  (insert "|STR|DEX|CON|INT|WIS|CHA|")
  (forward-line 1)
  (org-table-hline-and-move)
  (org-table-align)
  )
