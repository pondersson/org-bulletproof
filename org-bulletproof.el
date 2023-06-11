;;; org-bulletproof.el --- Automatic plain list bullet cycling -*- lexical-binding: t; -*-

;; Author: Pontus Andersson <pondersson@gmail.com>
;; Maintainer: Pontus Andersson <pondersson@gmail.com>
;; Homepage: https://github.com/pondersson/org-bulletproof
;; Version: 0.3
;; Package-Requires: ((emacs "27.1"))
;; Keywords: outlines, convenience

;; This program is free software: you can redistribute it and/or modify
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

;; This package adds a minor mode that advices some of Org mode's plain list formatting
;; functions to enable automatic bullet cycling in indented plain lists.  The user only
;; has to choose between unordered and ordered bullets.
;;
;; See README.org in the package repository for more details.

;;; Code:

(require 'org)

(defgroup org-bulletproof nil
  "Automatic plain list bullet cycling."
  :link '(url-link :tag "Homepage" "https://github.com/pondersson/org-bulletproof")
  :link '(emacs-library-link :tag "Library Source" "org-bulletproof.el")
  :group 'org
  :prefix "org-bulletproof-")

(defun org-bulletproof--set-cycle (symbol value)
  "Take a step toward ensuring the cycle can always provide a default bullet.

This check isn't complete because considering all combinations of
Org settings such as alphas and numbered item terminators as well
as contexts gets quite complicated.

If all is well, SYMBOL will be set to VALUE through `set-default-toplevel-value'"
  (when (= (length value) 0)
    (error (format "Invalid value: %s should contain at least one bullet." symbol)))
  (when (and (equal symbol 'org-bulletproof-unordered-cycle)
             (and (= (length value) 1) (equal (car value) "*")))
    (error (format "Invalid value: %s needs at least one non-* entry." symbol)))
  (set-default-toplevel-value symbol value))

(defcustom org-bulletproof-unordered-cycle
  '("-" "+" "*")
  "List containing the unordered bullet cycle.

In any given context, the first available bullet becomes the default."
  :group 'org-bulletproof
  :type '(repeat (choice (const "-") (const "+") (const "*")))
  :set #'org-bulletproof--set-cycle)

(defcustom org-bulletproof-ordered-cycle
  '("1." "1)")
  "List containing the ordered bullet cycle.

In any given context, the first available bullet becomes the default."
  :group 'org-bulletproof
  :type '(repeat (choice (const "1.") (const "1)")
                         (const "a.") (const "A.")
                         (const "a)") (const "A)")))
  :set #'org-bulletproof--set-cycle)

;;;###autoload
(define-minor-mode org-bulletproof-mode
  "Automatic plain list bullet cycling."
  :group 'org-bulletproof
  (when org-bulletproof-mode
    (advice-add #'org-cycle-list-bullet :filter-args #'org-bulletproof--filter)
    (advice-add #'org-list-struct-fix-bul :before #'org-bulletproof--force-bullets)))

;;;###autoload
(define-globalized-minor-mode global-org-bulletproof-mode
  org-bulletproof-mode org-bulletproof--on
  :group 'org-bulletproof)

(defun org-bulletproof--on ()
  "Enable `org-bulletproof-mode' in every Org buffer."
  (when (derived-mode-p #'org-mode)
    (org-bulletproof-mode +1)))

(defun org-bulletproof--filter (&optional which)
  "Toggle current plain list bullet between unordered and ordered.

Pass through the original WHICH argument when `org-bulletproof-mode' is nil."
  (if (and org-bulletproof-mode (org-at-item-p))
      (let* ((struct (org-list-struct))
             (item (org-list-get-item-begin))
             (bullet (org-trim (org-list-get-bullet item struct)))
             (type (org-bulletproof--get-type bullet))
             (new-type (if (equal type 'unordered) 'ordered 'unordered))
             (prevs (org-list-prevs-alist struct))
             (available-bullets (org-bulletproof--get-available-bullets item struct prevs))
             (new-bullet (org-bulletproof--get-default-bullet new-type available-bullets)))
        ;; Return a list because the `apply' call in the advice expects it
        (list new-bullet))
    which))

(defun org-bulletproof--force-bullets (struct prevs)
  "Force all list bullets according to set rules.

PREVS is the alist of previous items, as returned by
`org-list-prevs-alist'.  This function modifies STRUCT."
  (when org-bulletproof-mode
    (let ((parents (org-list-parents-alist struct)))
      ;; Forcing all bullets requires the changed sublist to be fully updated. Because this
      ;; runs before `org-list-struct-fix-bul', all Org mode has done so far is set the
      ;; first bullet of the changed sublist, so propagate that bullet.
      (org-bulletproof--propagate-first-sublist-bullet struct prevs)
      ;; Set each item's bullet according to its relationship with its parent
      (dolist (item (mapcar #'car struct))
        (org-bulletproof--force-bullet item struct prevs parents)))))

(defun org-bulletproof--propagate-first-sublist-bullet (struct prevs)
  "Set all bullets in the current sublist equal to the first bullet.

PREVS is the alist of previous items, as returned by
`org-list-prevs-alist'.  This function modifies STRUCT."
  (save-excursion
    (beginning-of-line)
    (let* ((sublist-items (org-list-get-all-items (point) struct prevs))
           (first-item (car sublist-items))
           (first-bullet (org-list-get-bullet first-item struct)))
      (dolist (item sublist-items)
        (org-list-set-bullet item struct first-bullet)))))

(defun org-bulletproof--force-bullet (item struct prevs parents)
  "Force ITEM's bullet based on its parent.

PREVS is the alist of previous items, as returned by
`org-list-prevs-alist', and PARENTS is the alist of parent items,
as returned by `org-list-parents-alist'.

This function modifies STRUCT."
  (let* ((bullet (org-trim (org-list-get-bullet item struct)))
         (type (org-bulletproof--get-type bullet))
         (parent (org-list-get-parent item struct parents))
         (parent-bullet (when parent (org-trim (org-list-get-bullet parent struct))))
         (parent-type (when parent-bullet (org-bulletproof--get-type parent-bullet)))
         ;; TODO(pontus): Share available bullets within each sublist.
         (available-bullets (org-bulletproof--get-available-bullets item struct prevs))
         (forced-bullet
          (if (equal type parent-type)
              (org-bulletproof--cycle-bullet parent-type parent-bullet available-bullets)
            (org-bulletproof--get-default-bullet type available-bullets))))
    (org-list-set-bullet item struct (org-list-bullet-string forced-bullet))))

;; Adapted from https://git.savannah.gnu.org/cgit/emacs/org-mode.git/tree/lisp/org-list.el
(defun org-bulletproof--get-available-bullets (item struct prevs)
  "List available bullets for ITEM in the current context.

PREVS is the alist of previous items, as returned by
`org-list-prevs-alist'.  This function modifies STRUCT."
  (save-excursion
    (goto-char item)
    (let* ((level-0-p (= 0 (org-list-get-nth 1 item struct)))
           (description-p (org-at-item-description-p))
           (list-beg (org-list-get-first-item item struct prevs))
           (alpha-p (org-list-use-alpha-bul-p list-beg struct prevs)))
      (append '("-" "+" )
              ;; *-bullets are not allowed at column 0
              (unless level-0-p '("*"))
              ;; Description items cannot be numbered
              (unless (or (eq org-plain-list-ordered-item-terminator ?\))
                          description-p)
                '("1."))
              (unless (or (eq org-plain-list-ordered-item-terminator ?.)
                          description-p)
                '("1)"))
              (unless (or (not alpha-p)
                          (eq org-plain-list-ordered-item-terminator ?\))
                          description-p)
                '("a." "A."))
              (unless (or (not alpha-p)
                          (eq org-plain-list-ordered-item-terminator ?.)
                          description-p)
                '("a)" "A)"))))))

(defun org-bulletproof--cycle-bullet (type parent-bullet available-bullets)
  "Cycle bullet within TYPE based on PARENT-BULLET, respecting AVAILABLE-BULLETS."
  (let* ((cycle (if (equal type 'unordered)
                    org-bulletproof-unordered-cycle
                  org-bulletproof-ordered-cycle))
         (subcycle (cdr (member parent-bullet cycle)))
         (candidates (seq-filter (lambda (b) (member b available-bullets)) subcycle))
         (cycled-bullet (car candidates)))
    (if cycled-bullet
        cycled-bullet
      (org-bulletproof--get-default-bullet type available-bullets))))


(defun org-bulletproof--get-default-bullet (type available-bullets)
  "Get the default bullet of TYPE taking AVAILABLE-BULLETS into account."
  (let* ((cycle (if (equal type 'unordered)
                    org-bulletproof-unordered-cycle
                  org-bulletproof-ordered-cycle))
         (candidates (seq-filter (lambda (b) (member b available-bullets)) cycle))
         (default-bullet (car candidates)))
    (unless default-bullet
        (error (format "No default bullet available in org-bulletproof-%s-cycle" type)))
    default-bullet))

(defun org-bulletproof--get-type (bullet)
  "Return the type of the BULLET string, either `unordered' or `ordered'."
  (if (string-match-p (org-bulletproof--ordered-terminator-re) bullet) 'ordered 'unordered))

(defun org-bulletproof--ordered-terminator-re ()
  "Return the regular expression for ordered item terminators."
  (cond
   ((eq org-plain-list-ordered-item-terminator t) "[.)]")
   ((= org-plain-list-ordered-item-terminator ?\)) ")")
   ((= org-plain-list-ordered-item-terminator ?.) "\\.")
   (t "[.)]")))

(provide 'org-bulletproof)

;;; org-bulletproof.el ends here
