;;; test-org-bulletproof.el --- Tests for org-bulletproof.el  -*- lexical-binding: t; -*-

;;; Code:

(require 'org-bulletproof)
(require 'org)
(require 'ert)

;; Taken from https://git.savannah.gnu.org/cgit/emacs/org-mode.git/tree/testing/org-test.el
(defmacro org-bulletproof-test-with-temp-text (text &rest body)
  "Run body in a temporary buffer with Org mode as the active
mode holding TEXT.  If the string \"<point>\" appears in TEXT
then remove it and place the point there before running BODY,
otherwise place the point at the beginning of the inserted text."
  (declare (indent 1) (debug t))
  `(let ((inside-text (if (stringp ,text) ,text (eval ,text)))
         (org-mode-hook nil))
     (with-temp-buffer
       (org-mode)
       (org-bulletproof-mode +1)
       (let ((point (string-match "<point>" inside-text)))
         (if point
             (progn
               (insert (replace-match "" nil nil inside-text))
               (goto-char (1+ (match-beginning 0))))
           (insert inside-text)
           (goto-char (point-min))))
       (font-lock-ensure (point-min) (point-max))
       ,@body)))

(ert-deftest test-org-bulletproof/cycle-unordered-bullet ()
  (org-bulletproof-test-with-temp-text "* H\n<point>- item"
    (org-shiftright)
    (should (looking-at "1) item"))))

(ert-deftest test-org-bulletproof/cycle-ordered-bullet ()
  (org-bulletproof-test-with-temp-text "* H\n<point>1) item"
    (org-shiftleft)
    (should (looking-at "- item"))))

(ert-deftest test-org-bulletproof/cycle-indented-unordered-bullet ()
  (org-bulletproof-test-with-temp-text "* H
- item 1
<point>- item 2"
    (org-metaright)
    (beginning-of-line)
    (should (looking-at "  \\+ item 2"))))

(ert-deftest test-org-bulletproof/cycle-indented-ordered-bullet ()
  (org-bulletproof-test-with-temp-text "* H
1) item 1
<point>2) item 2"
    (org-metaright)
    (beginning-of-line)
    (should (looking-at "   1\\. item 2"))))

(ert-deftest test-org-bulletproof/force-default-bullet ()
  (org-bulletproof-test-with-temp-text "* H\n<point>+ item"
    (org-ctrl-c-ctrl-c)
    (beginning-of-line)
    (should (looking-at "- item"))))


(ert-deftest test-org-bulletproof/nested ()
  (org-bulletproof-test-with-temp-text "* H
<point>1) item A
2) item B
   1. item C
   2. item D
      1) item F"
    (org-shiftright)
    (next-line 4)
    (beginning-of-line)
    (should (looking-at "     1\\. item F"))))

(ert-deftest test-org-bulletproof/readme-example ()
  (org-bulletproof-test-with-temp-text "* H
- foo
<point>- bar"
    (org-metaright)
    (beginning-of-line)
    (should (looking-at "  \\+ bar"))
    (previous-line)
    (org-shiftright)
    (beginning-of-line)
    (should (looking-at "1) foo\n   - bar"))
    (next-line)
    (org-shiftright)
    (beginning-of-line)
    (should (looking-at "   1\\. bar"))))

(defun benchmark-org-bulletproof/simple (arg)
  "Benchmark bullet cycling on a simple list with org-bulletproof-mode determined by ARG."
  (interactive "p")
  (let* ((list-fragment "- item A\n")
         (big-list (apply 'concat (make-list 50 list-fragment))))
    (org-bulletproof-test-with-temp-text big-list
      (org-bulletproof-mode arg)
      (benchmark 100 `(progn (org-shiftleft) (org-shiftright))))))

(defun benchmark-org-bulletproof/nested (arg)
  "Benchmark bullet cycling on a nested list with org-bulletproof-mode determined by ARG."
  (interactive "p")
  (let* ((list-fragment "- item A\n  1) item B\n     1. item C\n")
         (big-list (apply 'concat (make-list 10 list-fragment))))
    (org-bulletproof-test-with-temp-text big-list
      (org-bulletproof-mode arg)
      (benchmark 100 `(progn (org-shiftleft) (org-shiftright))))))
