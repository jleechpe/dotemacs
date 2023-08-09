(define-derived-mode hcl-ts-mode prog-mode "HCL"
  "
  \\{hcl-ts-mode-map}"
  :syntax-table hcl-mode-syntax-table
  (when (treesit-ready-p 'hcl)
    (treesit-parser-create 'hcl)
    (setq-local treesit-font-lock-feature-list
                '((comment definition)
                  (keyword string type)
                  (assignment builtin constant punctuation
                   escape-sequence number property string-interpolation)
                  (bracket delimiter function operator variable)))
    (setq-local treesit-font-lock-settings hcl--font-lock-settings)
    (setq-local imenu-create-index-function
                #'hcl-imenu-treesit-create-index)
    (setq-local treesit-defun-name-function
                #'hcl--treesit-defun-name)
    (treesit-major-mode-setup)
    ))


(defun hcl-imenu-treesit-create-index (&optional node)
  ""
  (interactive)
  (let* ((node (or node (treesit-buffer-root-node 'hcl)))
         (tree (treesit-induce-sparse-tree node (rx (seq bol (or "block" "attribute") eol)))))
    (hcl--imenu-treesit-create-index-1 tree)))

(defun hcl--imenu-treesit-create-index-1 (node)
  ""
  (let* ((ts-node (car node))
         (children (cdr node))
         (subtrees (mapcan #'hcl--imenu-treesit-create-index-1
                           children))
         (name (when ts-node
                 (or (hcl--treesit-defun-name ts-node)
                     "Anonymous")))
         (type (when ts-node
                 (hcl--treesit-node-type ts-node)))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (message "%S" name)
    (cond
     ((null ts-node)
      subtrees)
     (subtrees
      (let ((parent-label name)
            (jump-label type))
        `((,parent-label
           ,(cons jump-label marker)
           ,@subtrees))))
     (t (list (cons (format "(%s) %s" type name) marker))))))

(defun hcl--treesit-defun-name (node)
  (mapconcat 'identity (remove nil (mapcar (lambda (x)
            (pcase (treesit-node-type x)
              ("identifier" (treesit-node-text x t))
              ("string_lit" (treesit-node-text (treesit-node-child x 1) t))
              ("attribute" (treesit-node-text x t))
              ))
              (treesit-node-children node))) "."))

(defun hcl--treesit-node-type (node)
  (pcase (treesit-node-type node)
    ("block" (format "(block) %s"(treesit-node-text (treesit-node-child node 0) t)))
    ("attribute" "attribute")
    (_ "Something Went Wrong")))

(defvar hcl-ts-mode--punctuation
  '("," ":" "=")
  "Punctuation for HCL mode")

(defface font-lock-hcl-bool-face
  '((t :inherit font-lock-builtin-face))
  "Face for Booleans in HCL code")

(setq hcl--font-lock-settings
  (treesit-font-lock-rules
   :feature 'comment
   :language 'hcl
   '((comment) @font-lock-comment-face)

   :feature 'string
   :language 'hcl
   '((string_lit) @font-lock-string-face
     (quoted_template) @font-lock-string-face)

   :feature 'keyword
   :language 'hcl
   :override t
   '((function_call (identifier) @font-lock-function-call-face)
     ((for_intro (identifier) @font-lock-constant-face
                 (identifier) @font-lock-constant-face)
      @font-lock-function-name-face))

   :feature 'punctuation
   :language 'hcl
   :override t
   `(([,@hcl-ts-mode--punctuation]) @font-lock-punctuation-face)

   :feature 'string-interpolation
   :language 'hcl
   :override 't
   '((template_interpolation) @font-lock-type-face)

   :feature 'assignment
   :language 'hcl
   '((attribute (identifier) @font-lock-variable-name-face))

   :feature 'assignment
   :language 'hcl
   :override t
   '((numeric_lit) @font-lock-number-face
     ((bool_lit) @font-lock-hcl-bool-face))

   :feature 'assignment
   :language 'hcl
   :override nil
   '(((variable_expr) @font-lock-type-face)
     (object_elem key: (expression (variable_expr) @font-lock-variable-name-face))
     (expression (variable_expr) @font-lock-keyword-face
                (get_attr (identifier) @font-lock-variable-name-face)))


   :feature 'assignment
   :language 'hcl
   :override t
   '((block (identifier) @font-lock-keyword-face
            (string_lit) @font-lock-warning-face
            (string_lit) @font-lock-constant-face)
     (block (identifier) @font-lock-keyword-face
            (string_lit) @font-lock-constant-face)
     (block (body (block (identifier) @font-lock-keyword-face)))
     (block (identifier) @font-lock-keyword-face))
   ))

(define-derived-mode terraform-ts-mode hcl-ts-mode "Terraform")
;; Defun navigation

;; Electric pairs

;; Comments

;; indent level
(provide 'hcl-ts-mode)
