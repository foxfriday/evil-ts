;;; evil-ts.el --- Add actions to evil using treesit. -*- lexical-binding: t; -*-

;; Version: 0.0.4
;; URL: https://github.com/foxfriday/evil-ts
;; Package-Requires: ((emacs "29") (evil "1"))

;;; Commentary:
;; This package has the minor mode evil-ts-mode.  Activating the minor mode
;; adds some actions to evil mode.  There are some text objects and some
;; functions to move around nodes.

;;; Code:

(require 'evil)
(require 'rx)
(require 'treesit)

(defvar evil-ts-statement
  (rx bos
      (or "if" "for" "for_in" "try" "with" "while" "do" "switch" "match")
      "_statement" eos)
  "Regex matching the node type of a statement.")

(defvar evil-ts-function
  (rx bos
      (or "function_definition" "function_declaration" "function_item"
          "function_expression" "arrow_function" "method_definition"
          "method_declaration" "constructor_declaration" "method")
      eos)
  "Regex matching the node type of a function or method definition.")

(defvar evil-ts-class
  (rx bos
      (or "class_definition" "class_declaration" "class_specifier"
          "struct_specifier" "struct_item" "enum_item" "impl_item"
          "class" "module")
      eos)
  "Regex matching the node type of a class or similar definition.")

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
  (let ((node (treesit-thing-at-point obj 'nested)))
    (when node
      (list (treesit-node-start node) (treesit-node-end node)))))

(defun evil-ts-expand-region (&optional beg end)
  "Expand the region BEG...END to the closest strictly larger node.
BEG and END default to the position of point."
  (let* ((beg (or beg (point)))
         (end (or end (point)))
         (node (treesit-node-at beg)))
    (while (and node
                (not (and (<= (treesit-node-start node) beg)
                          (>= (treesit-node-end node) end)
                          (or (< (treesit-node-start node) beg)
                              (> (treesit-node-end node) end)))))
      (setq node (treesit-node-parent node)))
    (when node
      (list (treesit-node-start node) (treesit-node-end node)))))

(evil-define-text-object evil-ts-text-obj-stat (count &optional beg end type)
  (evil-ts-select-obj evil-ts-statement))

(evil-define-text-object evil-ts-text-obj-fun (count &optional beg end type)
  (evil-ts-select-obj evil-ts-function))

(evil-define-text-object evil-ts-text-obj-class (count &optional beg end type)
  (evil-ts-select-obj evil-ts-class))

(evil-define-text-object evil-ts-text-obj-expand-region (count &optional beg end type)
  (evil-ts-expand-region beg end))

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
  "Small integration between evil and the built-in tree-sitter.

The mode adds some text objects and some movements.  The text
objects are bound to `s' for a statement, `f' for a function and
`c' for a class, and work in visual and operator state.  So the
sequence `vaf' selects the surrounding function and `daf' deletes
it.  There is no difference between the inner and the outer
variant of an object.  The object `x' expands the selection to
the closest parent node, so after `vax' you can press `ax'
repeatedly to keep expanding.  You can also move to the beginning
or end of an object in normal state with the prefix `[' or `]'
indicating the direction.  So `[f' moves the cursor to the start
of the previous function.

Key bindings:
\\{evil-ts-mode-map}"
  :lighter " evil-ts"
  :keymap evil-ts-mode-map
  (when (and evil-ts-mode (not (treesit-available-p)))
    (setq evil-ts-mode nil)
    (user-error "This Emacs was not built with tree-sitter support")))

(evil-define-key '(visual operator) evil-ts-mode-map
  "as" 'evil-ts-text-obj-stat
  "is" 'evil-ts-text-obj-stat
  "af" 'evil-ts-text-obj-fun
  "if" 'evil-ts-text-obj-fun
  "ac" 'evil-ts-text-obj-class
  "ic" 'evil-ts-text-obj-class
  "ax" 'evil-ts-text-obj-expand-region
  "ix" 'evil-ts-text-obj-expand-region)

(evil-define-key '(normal visual) evil-ts-mode-map
  "[c" 'evil-ts-beginning-of-class
  "]c" 'evil-ts-end-of-class
  "[w" 'evil-ts-beginning-of-statement
  "]w" 'evil-ts-end-of-statement
  "[f" 'treesit-beginning-of-defun
  "]f" 'treesit-end-of-defun)

(provide 'evil-ts)
;;; evil-ts.el ends here
