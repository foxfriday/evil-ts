;;; evil-ts.el --- Add actions to evil using treesit -*- lexical-binding: t; -*-

;; Author: M. Rincon
;; Version: 0.1.0
;; URL: https://github.com/foxfriday/evil-ts
;; Package-Requires: ((emacs "30.1") (evil "1"))

;;; Commentary:
;; This package has the minor mode evil-ts-mode.  Activating the minor mode
;; adds some actions to evil mode.  There are some text objects and some
;; motions to move around nodes.

;;; Code:

(require 'evil)
(require 'rx)
(require 'treesit)

(defun evil-ts--named-p (node)
  "Return non-nil if NODE is a named node.
Keywords such as `lambda' or `class' are anonymous nodes whose
type is the same as the definition they start."
  (treesit-node-check node 'named))

(defun evil-ts--definition-p (node)
  "Return non-nil if NODE is a named type definition.
In C and C++, `struct foo x;' contains a `struct_specifier' node
that only refers to the type, and should not count as a class."
  (and (evil-ts--named-p node)
       (or (not (string-suffix-p "_specifier" (treesit-node-type node)))
           (treesit-node-child-by-field-name node "body"))))

(defvar evil-ts-statement
  (rx bos
      (or (seq (or "if" "for" "for_in" "c_style_for" "try" "with" "while"
                   "do" "switch" "match" "case")
               "_statement")
          "for_range_loop")
      eos)
  "Regex matching the node type of a statement.")

(defvar evil-ts-function
  (cons (rx bos
            (or "function_definition" "function_declaration" "function_item"
                "function_expression" "arrow_function" "method_definition"
                "method_declaration" "constructor_declaration" "method"
                "generator_function_declaration" "generator_function"
                "lambda" "lambda_expression")
            eos)
        #'evil-ts--named-p)
  "Thing matching a function or method definition.")

(defvar evil-ts-class
  (cons (rx bos
            (or "class_definition" "class_declaration" "class_specifier"
                "struct_specifier" "union_specifier" "enum_specifier"
                "struct_item" "enum_item" "impl_item" "class" "module")
            eos)
        #'evil-ts--definition-p)
  "Thing matching a class or similar definition.")

(defvar evil-ts-wrapper
  (rx bos
      (or "decorated_definition" "template_declaration" "export_statement")
      eos)
  "Regex matching the node type of a wrapper around a definition.")

;;; Motions

(defmacro evil-ts--define-motions (name thing)
  "Define motions to the start and end of the previous and next NAME.
THING is the variable holding the thing to move to."
  (let ((noun (symbol-name name)))
    `(progn
       (evil-define-motion ,(intern (format "evil-ts-previous-%s-start" noun))
         (count)
         ,(format "Move to the start of the COUNTth previous %s." noun)
         :jump t
         :type exclusive
         (treesit-beginning-of-thing ,thing (or count 1)))
       (evil-define-motion ,(intern (format "evil-ts-next-%s-start" noun))
         (count)
         ,(format "Move to the start of the COUNTth next %s." noun)
         :jump t
         :type exclusive
         (treesit-beginning-of-thing ,thing (- (or count 1))))
       (evil-define-motion ,(intern (format "evil-ts-previous-%s-end" noun))
         (count)
         ,(format "Move to the end of the COUNTth previous %s." noun)
         :jump t
         :type exclusive
         (treesit-end-of-thing ,thing (- (or count 1))))
       (evil-define-motion ,(intern (format "evil-ts-next-%s-end" noun))
         (count)
         ,(format "Move to the end of the COUNTth next %s." noun)
         :jump t
         :type exclusive
         (treesit-end-of-thing ,thing (or count 1))))))

(evil-ts--define-motions statement evil-ts-statement)
(evil-ts--define-motions function evil-ts-function)
(evil-ts--define-motions class evil-ts-class)

(define-obsolete-function-alias 'evil-ts-beginning-of-class
  #'evil-ts-previous-class-start "0.1.0")
(define-obsolete-function-alias 'evil-ts-end-of-class
  #'evil-ts-next-class-end "0.1.0")
(define-obsolete-function-alias 'evil-ts-beginning-of-statement
  #'evil-ts-previous-statement-start "0.1.0")
(define-obsolete-function-alias 'evil-ts-end-of-statement
  #'evil-ts-next-statement-end "0.1.0")

;;; Text objects

(defun evil-ts--outer (node)
  "Return NODE, or the outermost wrapper around it.
See `evil-ts-wrapper'."
  (let ((parent (treesit-node-parent node)))
    (while (and parent (treesit-node-match-p parent evil-ts-wrapper))
      (setq node parent
            parent (treesit-node-parent node)))
    node))

(defun evil-ts--range (node inner)
  "Return the range of NODE, or of its body if INNER is non-nil.
The outer range includes any wrapper around NODE.  Braces around
the body are not part of the inner range, and neither are `do'
and `done' nor the whitespace next to them, so the result is
still valid shell syntax.  If NODE has no body, the inner range
is that of NODE."
  (let* ((body (and inner
                    (or (treesit-node-child-by-field-name node "body")
                        (treesit-node-child-by-field-name node "consequence"))))
         (open (and body (treesit-node-child body 0)))
         (close (and body (treesit-node-child body -1))))
    (cond ((not inner)
           (setq node (evil-ts--outer node))
           (list (treesit-node-start node) (treesit-node-end node)))
          ((null body)
           (list (treesit-node-start node) (treesit-node-end node)))
          ((and (equal (treesit-node-type open) "{")
                (equal (treesit-node-type close) "}"))
           (list (treesit-node-end open) (treesit-node-start close)))
          ((and (equal (treesit-node-type open) "do")
                (equal (treesit-node-type close) "done")
                (> (treesit-node-child-count body) 2))
           (list (treesit-node-start (treesit-node-child body 1))
                 (treesit-node-end (treesit-node-child body -2))))
          (t
           (list (treesit-node-start body) (treesit-node-end body))))))

(defun evil-ts-select-obj (thing &optional count beg end inner)
  "Return the range of the COUNTth THING around point.
THING is anything `treesit-thing-at-point' accepts, such as a
regex matching the node type.  BEG and END delimit the current
selection, and things whose range does not enlarge it are
skipped, so repeating the text object keeps widening it.  If
INNER is non-nil, return the range of the body of the thing."
  (let ((beg (or beg (point)))
        (end (or end (point)))
        (node (treesit-thing-at-point thing 'nested)))
    (while (and node
                (not (let ((outer (evil-ts--range node nil))
                           (range (evil-ts--range node inner)))
                       (and (<= (car outer) beg)
                            (>= (cadr outer) end)
                            (or (< (car range) beg) (> (cadr range) end))))))
      (setq node (treesit-parent-until node thing)))
    (dotimes (_ (1- (abs (or count 1))))
      (when node
        (setq node (treesit-parent-until node thing))))
    (when node
      (evil-ts--range node inner))))

(defun evil-ts-expand-region (&optional beg end count)
  "Expand the region BEG...END to the COUNTth strictly larger node.
BEG and END default to the position of point."
  (let ((beg (or beg (point)))
        (end (or end (point)))
        (node t))
    (dotimes (_ (abs (or count 1)))
      (setq node (treesit-node-at beg))
      (while (and node
                  (not (and (<= (treesit-node-start node) beg)
                            (>= (treesit-node-end node) end)
                            (or (< (treesit-node-start node) beg)
                                (> (treesit-node-end node) end)))))
        (setq node (treesit-node-parent node)))
      (when node
        (setq beg (treesit-node-start node)
              end (treesit-node-end node))))
    (when node
      (list beg end))))

(evil-define-text-object evil-ts-text-obj-stat (count &optional beg end _type)
  "Select a statement."
  :type exclusive
  (evil-ts-select-obj evil-ts-statement count beg end))

(evil-define-text-object evil-ts-text-obj-inner-stat (count &optional beg end _type)
  "Select the body of a statement."
  :type exclusive
  (evil-ts-select-obj evil-ts-statement count beg end t))

(evil-define-text-object evil-ts-text-obj-fun (count &optional beg end _type)
  "Select a function."
  :type exclusive
  (evil-ts-select-obj evil-ts-function count beg end))

(evil-define-text-object evil-ts-text-obj-inner-fun (count &optional beg end _type)
  "Select the body of a function."
  :type exclusive
  (evil-ts-select-obj evil-ts-function count beg end t))

(evil-define-text-object evil-ts-text-obj-class (count &optional beg end _type)
  "Select a class."
  :type exclusive
  (evil-ts-select-obj evil-ts-class count beg end))

(evil-define-text-object evil-ts-text-obj-inner-class (count &optional beg end _type)
  "Select the body of a class."
  :type exclusive
  (evil-ts-select-obj evil-ts-class count beg end t))

(evil-define-text-object evil-ts-text-obj-expand-region (count &optional beg end _type)
  "Select the closest node larger than the selection."
  :type exclusive
  (evil-ts-expand-region beg end count))

;;; Mode

(defvar evil-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c t C" #'evil-ts-previous-class-start)
    (keymap-set map "C-c t c" #'evil-ts-next-class-end)
    (keymap-set map "C-c t F" #'evil-ts-previous-function-start)
    (keymap-set map "C-c t f" #'evil-ts-next-function-end)
    (keymap-set map "C-c t W" #'evil-ts-previous-statement-start)
    (keymap-set map "C-c t w" #'evil-ts-next-statement-end)
    map)
  "The keymap associated with `evil-ts-mode'.")

;;;###autoload
(define-minor-mode evil-ts-mode
  "Small integration between evil and the built-in tree-sitter.

The mode adds some text objects and some motions.  The text
objects are bound to `s' for a statement, `f' for a function and
`c' for a class, and work in visual and operator state.  So the
sequence `vaf' selects the surrounding function and `daf' deletes
it.  The inner variant selects the body of the object.  Repeating
an object in visual state, or giving it a count, selects the
enclosing one.  The object `x' expands the selection to the
closest parent node, so after `vax' you can press `ax' repeatedly
to keep expanding.

The mode can only be enabled in buffers with a tree-sitter parser.

Key bindings:
\\{evil-ts-mode-map}"
  :lighter " evil-ts"
  :keymap evil-ts-mode-map
  (when (and evil-ts-mode
             (not (and (treesit-available-p) (treesit-parser-list))))
    (setq evil-ts-mode nil)
    ;; Only complain when called directly, so the mode can be added to
    ;; `prog-mode-hook' and stay off in buffers without a parser.
    (when (eq this-command 'evil-ts-mode)
      (user-error "This buffer has no tree-sitter parser")))
  (when (bound-and-true-p evil-local-mode)
    (evil-normalize-keymaps)))

(evil-define-key '(visual operator) evil-ts-mode-map
  "as" #'evil-ts-text-obj-stat
  "is" #'evil-ts-text-obj-inner-stat
  "af" #'evil-ts-text-obj-fun
  "if" #'evil-ts-text-obj-inner-fun
  "ac" #'evil-ts-text-obj-class
  "ic" #'evil-ts-text-obj-inner-class
  "ax" #'evil-ts-text-obj-expand-region
  "ix" #'evil-ts-text-obj-expand-region)

(evil-define-key '(normal visual operator) evil-ts-mode-map
  "[c" #'evil-ts-previous-class-start
  "]c" #'evil-ts-next-class-start
  "[C" #'evil-ts-previous-class-end
  "]C" #'evil-ts-next-class-end
  "[f" #'evil-ts-previous-function-start
  "]f" #'evil-ts-next-function-start
  "[F" #'evil-ts-previous-function-end
  "]F" #'evil-ts-next-function-end
  "[w" #'evil-ts-previous-statement-start
  "]w" #'evil-ts-next-statement-start
  "[W" #'evil-ts-previous-statement-end
  "]W" #'evil-ts-next-statement-end)

(provide 'evil-ts)
;;; evil-ts.el ends here
