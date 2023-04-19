;;; evil-ts.el --- Add actions to evil using treesit. -*- lexical-binding: t; -*-

;; Version: 0.0.01
;; URL: https://github.com/foxfriday/evil-ts
;; Package-Requires: ((emacs "29") (evil "1"))

;;; Commentary:
;; This package has the minor mode evil-ts-mode. Activating the minor mode
;; add some actions to evil mode. There are some text objects and some
;; functions to move around nodes.

;;; Code:

(require 'evil)
(require 'rx)
(require 'treesit)

(defvar evil-ts-statement (rx (or "if" "for" "try" "with" "while") "_statement")
  "Regex used to move to next or last statement.")

(defvar evil-ts-function "function_definition"
  "Regex used to move to next or last class.")

(defvar evil-ts-class "class_definition"
  "Regex used to move to next or last class.")

(defun evil-ts-beginning-of-class ()
  "Move to the start of a class definition."
  (interactive)
  (treesit-beginning-of-thing evil-ts-class))

(defun evil-ts-end-of-class ()
  "Move to the end of a class definition."
  (interactive)
  (treesit-end-of-thing evil-ts-class))

(defun evil-ts-beginning-of-statement ()
  "Move to the start of a statement definition."
  (interactive)
  (treesit-beginning-of-thing evil-ts-statement))

(defun evil-ts-end-of-statement ()
  "Move to the end of a statement definition."
  (interactive)
  (treesit-end-of-thing evil-ts-statement))

(defun evil-ts-select-obj (obj)
  "Select the region described by OBJ."
  (let* ((node (treesit-thing-at-point obj 'nested))
         (start (if node (treesit-node-start node) nil))
         (end (if node (treesit-node-end node) nil)))
    (when node
      (goto-char end)
      (list start end))))

(defun evil-ts-expand-region ()
  "Expand selection to the closet parent."
  (let* ((point (point))
         (mark (or (mark t) point))
         (start (min point mark))
         (end (max point mark))
         (node (treesit-node-at start))
         (parent (treesit-parent-until node
                                       (lambda (n) (and (> start (treesit-node-start  n))
                                                        (< end (treesit-node-end n))))
                                       nil))
         (pstart (if parent (treesit-node-start parent) nil))
         (pend (if parent (treesit-node-end parent) nil)))
    (when parent
      (goto-char pstart)
      (list pstart pend))))

(evil-define-text-object evil-ts-text-obj-stat (count &optional beg end type)
  (evil-ts-select-obj evil-ts-statement))

(evil-define-text-object evil-ts-text-obj-fun (count &optional beg end type)
  (evil-ts-select-obj evil-ts-function))

(evil-define-text-object evil-ts-text-obj-class (count &optional beg end type)
  (evil-ts-select-obj evil-ts-class))

(evil-define-text-object evil-ts-text-obj-expand-region (count &optional beg end type)
  (evil-ts-expand-region))

(defvar evil-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c t C" 'evil-ts-beginning-of-class)
    (keymap-set map "C-c t c" 'evil-ts-end-of-class)
    (keymap-set map "C-c t F" 'treesit-beginning-of-defun)
    (keymap-set map "C-c t f" 'treesit-end-of-defun)
    (keymap-set map "C-c t W" 'evil-ts-beginning-of-statement)
    (keymap-set map "C-c t w" 'evil-ts-end-of-statement)
    map)
  "The keymap associated with `evil-ts-mode'.")

;;;###autoload
(define-minor-mode evil-ts-mode
  "Small integration between evil and the build-in tree-sitter.

The mode adds some text objects and some movements. Text objects
are most useful when selecting an object in visual state. By
default, in visual state, `s` selects a statement, `f` a function
and `c` a class. So the sequence `vaf` will select the
surrounding function. You can also move to the last or previous
object in normal state using the same letters with the prefix `[`
or `]` indicating the direction. So, in normal state, `[f` moves
the cursor to the start of the previous function.

Key bindings:
\\{evil-ts-mode-map}"
  :init-value nil
  :lighter " evil-ts"
  :require 'treesit
  :keymap 'evil-ts-mode-map
  (unless (treesit-available-p)
    (error "Tree sitter does not seem to be available for this mode")))


(keymap-set evil-inner-text-objects-map "s" 'evil-ts-text-obj-stat)
(keymap-set evil-outer-text-objects-map "s" 'evil-ts-text-obj-stat)

(keymap-set evil-inner-text-objects-map "f" 'evil-ts-text-obj-fun)
(keymap-set evil-outer-text-objects-map "f" 'evil-ts-text-obj-fun)

(keymap-set evil-inner-text-objects-map "c" 'evil-ts-text-obj-class)
(keymap-set evil-outer-text-objects-map "c" 'evil-ts-text-obj-class)

(keymap-set evil-inner-text-objects-map "x" 'evil-ts-text-obj-expand-region)
(keymap-set evil-outer-text-objects-map "x" 'evil-ts-text-obj-expand-region)

(evil-define-key 'normal 'evil-ts-mode-map "[c" 'evil-ts-beginning-of-class)
(evil-define-key 'normal 'evil-ts-mode-map "]c" 'evil-ts-end-of-class)

(evil-define-key 'normal 'evil-ts-mode-map "[w" 'evil-ts-beginning-of-statement)
(evil-define-key 'normal 'evil-ts-mode-map "]w" 'evil-ts-end-of-statement)

(evil-define-key 'normal 'evil-ts-mode-map "[f" 'treesit-beginning-of-defun)
(evil-define-key 'normal 'evil-ts-mode-map "]f" 'treesit-end-of-defun)

(provide 'evil-ts)
;;; evil-ts.el ends here
