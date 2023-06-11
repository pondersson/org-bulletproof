;;; org-bulletproof-test.el --- Tests for org-bulletproof.el  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'org-bulletproof)

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

(ert-deftest test-org-bulletproof/default-environment ()
  (let ((org-bulletproof-unordered-cycle '("-" "+" "*"))
        (org-bulletproof-ordered-cycle '("1." "1)"))
        (org-list-allow-alphabetical nil)
        (org-plain-list-ordered-item-terminator t))
    ;; Cycle unordered bullet
    (org-bulletproof-test-with-temp-text "* H\n<point>- item"
      (org-shiftright)
      (should (looking-at "1\\. item")))
    ;; Cycle ordered bullet
    (org-bulletproof-test-with-temp-text "* H\n<point>1) item"
      (org-shiftleft)
      (should (looking-at "- item")))
    ;; Cycle indented unordered bullet
    (org-bulletproof-test-with-temp-text "* H
- item 1
<point>- item 2"
      (org-metaright)
      (beginning-of-line)
      (should (looking-at "  \\+ item 2")))
    ;; Cycle indented ordered bullet
    (org-bulletproof-test-with-temp-text "* H
1. item 1
<point>2. item 2"
      (org-metaright)
      (beginning-of-line)
      (should (looking-at "   1) item 2")))
    ;; Force default bullets
    (org-bulletproof-test-with-temp-text "* H\n<point>+ item"
      (org-ctrl-c-ctrl-c)
      (beginning-of-line)
      (should (looking-at "- item")))
    ;; Cycle all the way into nested sublists
    (org-bulletproof-test-with-temp-text "* H
<point>1. item 1
2. item 2
   1) item 3
   2) item 4
      1. item 5"
      (org-shiftright)
      (forward-line 4)
      (should (looking-at "     1) item 5")))
    ;; Don't cycle to numbered bullets on description items
    (org-bulletproof-test-with-temp-text "* H
<point>- description :: item 1"
      (should-error (org-shiftright)))
    ;; Respect counters
    (org-bulletproof-test-with-temp-text "* H
- item 1
<point>- [@10] item 2"
      (org-shiftright)
      (message (buffer-string))
      (should (looking-at "10\\. \\[@10\\] item 2")))
    ;; Ensure the README example works
    (org-bulletproof-test-with-temp-text "* H
- foo
<point>- bar"
      (org-metaright)
      (beginning-of-line)
      (should (looking-at "  \\+ bar"))
      (forward-line -1)
      (org-shiftright)
      (beginning-of-line)
      (should (looking-at "1\\. foo\n   - bar"))
      (forward-line)
      (org-shiftright)
      (beginning-of-line)
      (should (looking-at "   1) bar")))))

(ert-deftest test-org-bulletproof/alpha-numeric-mix ()
  ;; Handle a custom cycle with a mix of numbered and alpha bullets
  (let ((org-bulletproof-ordered-cycle '("1)" "a)"))
        (org-list-allow-alphabetical t)
        (org-plain-list-ordered-item-terminator t))
    (org-bulletproof-test-with-temp-text "* H
1) item 1
<point>2) item 2"
      (org-metaright)
      (beginning-of-line)
      (should (looking-at "   a) item 2")))))

(ert-deftest test-org-bulletproof/error-on-no-alpha ()
  ;; Error when alphas are disabled and the ordered alist contains nothing but alphas
  (let ((org-bulletproof-ordered-cycle '("a." "a)"))
        (org-list-allow-alphabetical nil)
        (org-plain-list-ordered-item-terminator t))
    (org-bulletproof-test-with-temp-text "* H\n<point>- item"
      (should-error (org-shiftright)))))

(ert-deftest test-org-bulletproof/level-0-default-* ()
  ;; Don't use "*" bullets as default at level 0
  (let ((org-bulletproof-unordered-cycle '("*" "-"))
        (org-bulletproof-ordered-cycle '("1." "1)"))
        (org-plain-list-ordered-item-terminator t))
    (org-bulletproof-test-with-temp-text "* H
<point>1. item 1"
      (org-shiftright)
      (beginning-of-line)
      (should (looking-at "- item 1")))))

(ert-deftest test-org-bulletproof/single-entry-cycle ()
  ;; Ensure cycling works with a single entry in the cycle
  (let ((org-bulletproof-unordered-cycle '("-")))
    (should (string= "* H
- item 1
  - item 2
    - item 3"
                     (org-bulletproof-test-with-temp-text "* H
- item 1
<point>- item 2
- item 3"
                       (org-metaright)
                       (forward-line)
                       (org-metaright)
                       (org-metaright)
                       (buffer-string))))))

(ert-deftest test-org-bulletproof/non-t-terminator ()
  ;; Respect non-t org-plain-list-ordered-item-terminator
  (let ((org-bulletproof-ordered-cycle '("1." "1)" "a." "A." "a)" "A)"))
        (org-list-allow-alphabetical t)
        (org-plain-list-ordered-item-terminator ?\)))
    (should (string= "* H
1) item 1
   a) item 2
      A) item 3"
                     (org-bulletproof-test-with-temp-text "* H
<point>- item 1
- item 2
- item 3"
                       (org-shiftright)
                       (forward-line)
                       (org-metaright)
                       (forward-line)
                       (org-metaright)
                       (org-metaright)
                       (buffer-string))))))

(ert-deftest test-org-bulletproof/alpha-26 ()
  ;; Use alpha bullets if list length is <=26
  (let ((org-bulletproof-ordered-cycle '("a." "1."))
        (org-list-allow-alphabetical t))
    (should (string= "* H
a. item 1
   1. item 2
b. item 3
c. item 4
d. item 5
e. item 6
f. item 7
g. item 8
h. item 9
i. item 10
j. item 11
k. item 12
l. item 13
m. item 14
n. item 15
o. item 16
p. item 17
q. item 18
r. item 19
s. item 20
t. item 21
u. item 22
v. item 23
w. item 24
x. item 25
y. item 26
z. item 27"
                     (org-bulletproof-test-with-temp-text "* H
1. item 1
<point>2. item 2
3. item 3
4. item 4
5. item 5
6. item 6
7. item 7
8. item 8
9. item 9
10. item 10
11. item 11
12. item 12
13. item 13
14. item 14
15. item 15
16. item 16
17. item 17
18. item 18
19. item 19
20. item 20
21. item 21
22. item 22
23. item 23
24. item 24
25. item 25
26. item 26
27. item 27"
                       (org-metaright)
                       (buffer-string))))))

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
