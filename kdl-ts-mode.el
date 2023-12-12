;;; kdl-ts-mode.el --- KDL Treesitter Mode           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Jonathan Leech-Pepin

;; Author: Jonathan Leech-Pepin <jonathan@leechpepin.com>
;; Keywords: languages, files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; Variables

(defvar kdl-ts-mode--punctuation
  '(";" "=")
  "KDL Treesitter Punctuation Characters")

(defvar kdl-ts-mode--brackets
  '("{" "}" "(" ")")
  "KDL Treesitter Bracket Characters")

;; Faces

(defface font-lock-kdl-bool-face
  '((t :inherit font-lock-constant-face))
  "KDL Treesitter Boolean face")

(defface font-lock-kdl-comment-delimiter-face
  '((t :inherit font-lock-comment-delimiter-face :bold t))
  "KDL Treesitter Comment Delimiter face")

;; Constants

(defconst kdl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (prog1
        table
      ;; Comments
      (modify-syntax-entry ?/ "_ 124b" table)
      (modify-syntax-entry ?\n "> b" table)
      (modify-syntax-entry ?* "_ 23b" table))))

;; Handle KDL files
;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.kdl") 'kdl-ts-mode))

(define-derived-mode kdl-ts-mode prog-mode "KDL"
  "
  \\{kdl-ts-mode-map}"
  :syntax-table kdl-mode-syntax-table
  (when (treesit-ready-p 'kdl)
    (treesit-parser-create 'kdl)
    (setq-local treesit-font-lock-feature-list
                '((comment definition)
                  (keyword string type)
                  (assignment builtin constant punctuation
                   escape-sequence number property string-interpolation)
                  (bracket delimiter function operator variable)))
    (setq-local treesit-font-lock-settings kdl--font-lock-settings)
    (setq-local treesit-simple-indent-rules kdl--treesit-simple-indent-rules)
    (setq-local imenu-create-index-function
                #'kdl-imenu-treesit-create-index)
    (setq-local treesit-defun-name-function
                #'kdl--treesit-defun-name)
    ;; Comment Logic
    (setq-local comment-start "//")
    (setq-local comment-start-skip "///*[ \t]*")
    (setq-local comment-use-syntax t)

    (treesit-major-mode-setup)))

;; Imenu Support
(defun kdl-imenu-treesit-create-index (&optional node)
  "Create imenu index"
  (interactive)
  (let* ((node (or node (treesit-buffer-root-node 'kdl)))
         (tree (treesit-induce-sparse-tree
                node (rx (seq bol (or "node" "node_field") eol)))))
    (kdl--imenu-treesit-create-index-1 tree)))

(defun kdl--imenu-treesit-create-index-1 (node)
  ""
  (let* ((ts-node (car node))
         (children (cdr node))
         (subtrees (mapcan #'kdl--imenu-treesit-create-index-1
                           children))
         (name (when ts-node
                 (kdl--treesit-defun-name ts-node)))
         (type (when ts-node
                 (kdl--treesit-node-type ts-node)))
         (loc (when ts-node
                (treesit-node-start ts-node)))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (cond
     ((null ts-node)
      subtrees)
     (subtrees
      (let ((parent-label (format "%s" name))
            (jump-label type))
        `((,parent-label
           ,(cons jump-label marker)
           ,@subtrees))))
     (t (cond
         ((string= "" name)
          (list (cons type marker)))
         ((string= type name)
          (list (cons name marker)))
         (t
          (list (cons (format "(%s) %s" type name) marker))))))))

(defun kdl--treesit-defun-name (node)
  "Treesitter defun name for kdl"
  (mapconcat 'identity
             (remove nil
                     (mapcar (lambda (x)
                               (pcase (treesit-node-type x)
                                 ("identifier" (treesit-node-text x t))))
                             (treesit-node-children node))) " "))

(defun kdl--treesit-node-type (node)
  (pcase (treesit-node-type node)
    ("node" (format "%s" (treesit-node-text (treesit-node-child node 0) t)))
    ("node_field"
     (format "%s" (treesit-node-text (treesit-node-child
                                      (treesit-node-child node 0) 0) t)))
    (_ "Oops")))

;; Indent rules
(setq kdl--treesit-simple-indent-rules
      '((kdl
         ((node-is "}") grand-parent 0)
         ((parent-is "node_children") grand-parent 4)
         ((node-is "single_line_comment") column-0 0)
         ((node-is "multi_line_comment") column-0 0)
         (no-node parent 0))))

;; Font Lock Settings
(setq kdl--font-lock-settings
      (treesit-font-lock-rules
       :feature 'comment
       :language 'kdl
       :override 'append
       `(((single_line_comment "//" @font-lock-kdl-comment-delimiter-face)
          @font-lock-comment-face)
         ((multi_line_comment) @font-lock-comment-face)
         ((node (node_comment) @font-lock-kdl-comment-delimiter-face)
          @font-lock-comment-face)
         ((node_children
           (node_children_comment)
           @font-lock-kdl-comment-delimiter-face)
          @font-lock-comment-face))

       :feature 'type
       :language 'kdl
       '((type (identifier) @font-lock-type-face))

       :feature 'keyword
       :language 'kdl
       '((node (identifier) @font-lock-keyword-face)
         ((boolean) @font-lock-kdl-bool-face))

       :feature 'string
       :language 'kdl
       :override 'append
       '(((string) @font-lock-string-face)
         (string_fragment (escape) @font-lock-escape-face))

       :feature 'punctuation
       :language 'kdl
       :override t
       `((([,@kdl-ts-mode--punctuation]) @font-lock-punctuation-face)
         (([,@kdl-ts-mode--brackets]) @font-lock-bracket-face))

       :feature 'number
       :language 'kdl
       '((number) @font-lock-number-face)

       :feature 'property
       :language 'kdl
       '((prop (identifier) @font-lock-property-name-face))))

(provide 'kdl-ts-mode)
;;; kdl-ts-mode.el ends here
