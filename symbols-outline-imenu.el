;;; symbols-outline-imenu.el --- Imenu backend for symbols-outline  -*- lexical-binding: t; -*-

;; Author: Shihao Liu
;; Keywords: outlines
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/liushihao456/symbols-outline.el

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Imenu backend that retrieves the document symbols with the imenu index of
;; the origin buffer.  It needs no external program, and it works for every
;; mode that provides imenu support.  For `emacs-lisp-mode' it covers more
;; definitions than the ctags backend, e.g. the whole `cl-defun' family.
;;
;; The node kinds are the ones imenu itself uses: definitions that imenu groups
;; into a menu (e.g. "Variables", "Types") get the kind of that menu, the
;; remaining ones are functions.  Menus that merely add a hierarchy level, as
;; for classes in some modes, become parent nodes.

;;; Code:

(require 'imenu)
(require 'symbols-outline-node)

(defvar symbols-outline--origin)

(defvar symbols-outline-imenu-menu-kind-alist
  '(("Variables" . "variable")
    ("Types" . "class")
    ("Functions" . "function")
    ("Macros" . "macro"))
  "Alist that maps imenu menu names to node kinds.

Definitions that are not in a menu get the kind `function'.  A menu name
missing here is a hierarchy level of the index itself, e.g. a class, and
its definitions get the kind of their parent.")

(defun symbols-outline-imenu--kind (menu-name fallback)
  "Return the node kind of imenu menu MENU-NAME, or FALLBACK if unknown."
  (or (cdr (assoc menu-name symbols-outline-imenu-menu-kind-alist)) fallback))

(defun symbols-outline-imenu--line (entry)
  "Return the line number of imenu ENTRY, or nil if it has no position."
  (let ((pos (cdr entry)))
    (cond ((markerp pos) (line-number-at-pos pos))
          ((integerp pos) (line-number-at-pos (copy-marker pos))))))

(defun symbols-outline-imenu--nodes (entries kind parent)
  "Return the nodes of imenu ENTRIES of KIND, attached to PARENT."
  (let (nodes)
    (dolist (entry entries)
      (unless (equal entry imenu--rescan-item)
        (if (imenu--subalist-p entry)
            ;; A menu is either a kind (handled by `kind') or a hierarchy level
            ;; of the index, e.g. a class; in the latter case it becomes a node
            ;; of its own, holding the entries of the menu.
            (let* ((menu (car entry))
                   (menu-kind (symbols-outline-imenu--kind menu kind))
                   (node (make-symbols-outline-node :name menu
                                                    :kind menu-kind
                                                    :parent parent)))
              (setf (symbols-outline-node-children node)
                    (symbols-outline-imenu--nodes (cdr entry) menu-kind node))
              (setf (symbols-outline-node-line node)
                    (apply #'min
                           (delq nil (mapcar #'symbols-outline-node-line
                                             (symbols-outline-node-children node)))))
              (push node nodes))
          (push (make-symbols-outline-node :name (car entry)
                                           :kind kind
                                           :line (symbols-outline-imenu--line entry)
                                           :parent parent)
                nodes))))
    (nreverse nodes)))

(defun symbols-outline-imenu--index-to-tree (index)
  "Convert imenu INDEX to a tree of `symbols-outline-node'."
  (let ((root (make-symbols-outline-node)))
    (setf (symbols-outline-node-children root)
          (symbols-outline-imenu--nodes index "function" root))
    (symbols-outline-node--sort-children root)
    root))

;;;###autoload
(defun symbols-outline-imenu-fetch (refresh-fn)
  "Retrieve symbols with imenu.
Argument REFRESH-FN should be called upon the retrieved symbols tree."
  (funcall refresh-fn
           (with-current-buffer symbols-outline--origin
             ;; Bind the index so that it's built from the current buffer
             ;; content instead of being reused from a previous fetch.  A mode
             ;; without imenu support signals `imenu-unavailable', in which
             ;; case the outline stays empty.
             (let ((imenu--index-alist nil))
               (symbols-outline-imenu--index-to-tree
                (ignore-errors (imenu--make-index-alist t)))))))

(provide 'symbols-outline-imenu)

;;; symbols-outline-imenu.el ends here
