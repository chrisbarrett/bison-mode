;;; bison-mode.el --- Major mode for editing bison/yacc files

;; Copyright (C) 1998 Eric Beuscher

;; Author:   Eric Beuscher <beuscher@eecs.tulane.edu>
;; Created:  2 Feb 1998
;; Version:  0.2
;; Package-Requires: ((cl-lib "0.2") (dash "2.5.0") (s "1.7.0"))
;; Keywords: bison-mode, yacc-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:
;;
;; Provides a major-mode for bison (.y) files.

;; A bison file is divided into a number of sections. Many commands in this
;; package perform a different actions depending on the current section.

;; bison-mode's indentation commands are context-sensitive. If point is in C
;; code the mode falls back the normal indentation behaviour for C.

;;; Code:

(eval-and-compile
  ;; Add cask packages to load path so flycheck checkers work.
  (when (boundp 'flycheck-emacs-lisp-load-path)
    (dolist (it (file-expand-wildcards "./.cask/*/elpa/*"))
      (add-to-list 'flycheck-emacs-lisp-load-path it))))

(require 'cl-lib)
(require 'cc-mode)
(require 'dash)
(require 's)

(defgroup bison-mode nil
  "Major mode for editing bison and yacc files"
  :group 'languages
  :prefix "bison-")

(defcustom bison-rule-separator-column 8
  "Column for rule and production separators \"|\" and \";\"."
  :group 'bison-mode
  :type 'integer)

(defcustom bison-rule-enumeration-column 16
  "Column for beginning enumeration of a production's rules."
  :group 'bison-mode
  :type 'integer)

(defcustom bison-decl-type-column 8
  "Column at which token and state types should be declared."
  :group 'bison-mode
  :type 'integer)

(defcustom bison-decl-token-column 24
  "Column at which tokens and states are listed when declared.

Used for %token, %type, etc."
  :group 'bison-mode
  :type 'integer)

(defcustom bison-disable-electric-keys? nil
  "Non-nil means all electric keys will be disabled."
  :group 'bison-mode
  :type 'boolean)

;;;; Internal Variables

(defvar bison--declarations
  '("%define" "%union" "%token" "%type" "%left" "%right" "%nonassoc")
  "List of commands which can declare a token or state type.")

(defvar bison--word-constituent-re (rx (or word "_")))

(defvar bison--production-re
  (eval `(rx bol (+ (regexp ,bison--word-constituent-re)) ":")))

(defvar bison--pre-c-decls-section 0
  "Section before c-declarations-section, if that section exists.")

(defvar bison--c-decls-section 1
  "Section denoted by %{ and $} for c-declarations at the top of a bison file.")

(defvar bison--bison-decls-section 2
  "Section before the rules section.")

(defvar bison--grammar-rules-section 3
  "Section delimited by %%'s where productions and rules are enumerated.")

(defvar bison--c-code-section 4
  "Section after the second %% where c-code can be placed.")

(defvar bison--c-decls-section-opener "%{")

(defvar bison--c-decls-section-closer "%}")

(defvar bison--grammar-rules-section-delimeter "%%")

(defvar bison-font-lock-keywords-1 c-font-lock-keywords
  "Basic highlighting for Bison mode.")

(defvar bison-font-lock-keywords-2
  (append
   (list
    (cons (eval `(rx bol (group-n 1 (or ,@bison--declarations))))
          '(1 font-lock-keyword-face)))
   bison-font-lock-keywords-1)
  "Gaudy highlighting for Bison mode.")

(defvar bison-font-lock-keywords bison-font-lock-keywords-2
  "Default expressions to highlight in Bison mode.")

;;;; Utility Functions

(defun bison--any-non-spaces-on-line-before-point? ()
  "Non-nil if there are non-whitespace chars between bol and point."
  (s-matches? (rx (not space))
              (buffer-substring (line-beginning-position) (point))))

(defun bison--any-non-spaces-on-line-after-point? ()
  "Non-nil if there are non-whitespace characters on this line."
  (s-matches? (rx (not space))
              (buffer-substring (point) (line-end-position))))

(defun bison--current-line ()
  "Return the current line as a string."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun bison--blank-line? ()
  "Non-nil if the current line is empty or entirely whitespace."
  (s-blank? (s-trim (bison--current-line))))

;;;; Section Parsers

(defun bison--section-start ()
  "Return the start of the section at point."
  (save-excursion
    (let ((bound (point)))
      (goto-char (point-min))
      (if (re-search-forward
           (concat "^" bison--c-decls-section-opener)
           bound t)
          (if (re-search-forward
               (concat "^" bison--c-decls-section-closer)
               bound t)
              (if (re-search-forward
                   (concat "^" bison--grammar-rules-section-delimeter)
                   bound t)
                  (if (re-search-forward
                       (concat "^" bison--grammar-rules-section-delimeter)
                       bound t)
                      bison--c-code-section
                    bison--grammar-rules-section)
                bison--bison-decls-section)
            bison--c-decls-section)
        (if (re-search-forward
             (concat "^" bison--grammar-rules-section-delimeter)
             bound t)
            (if (re-search-forward
                 (concat "^" bison--grammar-rules-section-delimeter)
                 bound t)
                bison--c-code-section
              bison--grammar-rules-section)
          (if (re-search-forward
               (concat "^" bison--c-decls-section-opener)
               nil t)
              bison--pre-c-decls-section
            (if (re-search-forward
                 (concat "^" bison--grammar-rules-section-delimeter)
                 nil t)
                bison--bison-decls-section
              bison--pre-c-decls-section)))))))

;;;; Syntax Parsers

(defun bison--after-production? ()
  "Non-nil if point is immediately after a production."
  (save-excursion
    (let ((pt (point)))
      (goto-char (line-beginning-position))
      (-when-let (prod-end (re-search-forward bison--production-re pt t))
        (unless (s-matches? (rx space)
                            (buffer-substring (line-beginning-position)
                                              (point)))
          (= prod-end pt))))))

(defun bison--production-start-pos ()
  "Try to move to the start of the production before point.
Return the buffer position on success."
  (save-excursion
    (re-search-backward bison--production-re nil t)))

(defun bison--next-production-pos ()
  "Return the position of the next production after point.
Return nil if there are no more productions."
  (save-excursion
    (when (re-search-forward bison--production-re nil t)
      (line-beginning-position))))

(defun bison--grammar-end-pos ()
  "Return the position of the end of the grammar rules.
Return nil if the end cannot be found."
  (save-excursion
    (when (re-search-forward
           (concat "^" bison--grammar-rules-section-delimeter)
           nil t)
      (line-beginning-position))))

(defun bison--grammar-section-start-pos ()
  "Return the position of the beginning of the grammar rules.
Return nil if the start cannot be found."
  (save-excursion
    (re-search-backward (concat "^" bison--grammar-rules-section-delimeter)
                        nil t)))

(defun bison--at-unfinished-production? ()
  "Non-nil if point is at an unfinished production."
  ;; Point is at an unfinished production if there is some non-whitespace text
  ;; before the next production or the end of the grammar section.
  (save-excursion
    (-when-let* ((end (or (bison--next-production-pos)
                          (bison--grammar-end-pos)))
                 (pos (re-search-forward
                       (rx-to-string `(+ (or space
                                             (and ,comment-start
                                                  (*? anything)
                                                  ,comment-end))))
                       end t)))
      (not (= pos end)))))

(defun bison--between-delimiters? (start-regexp end-regexp)
  "Non-nil if point is between START-REGEXP and END-REGEXP."
  (save-excursion
    (let ((limit (point)))
      (when (re-search-backward start-regexp nil t)
        (goto-char (match-end 0))
        (not (re-search-forward end-regexp limit t))))))

(defun bison--in-c-comment? ()
  "Non-nil if point is inside a C comment delimited by \"/*\" \"*/\"."
  (bison--between-delimiters? (regexp-quote comment-start)
                              (regexp-quote comment-end)))

(cl-defun bison--in-string? (&optional (pos (point)))
  "Non-nil if POS is inside a string."
  ;; Traverse the buffer for point-min, searching for unescaped quotes. If there
  ;; is an odd number then assume we are inside a string.
  (save-excursion
    (goto-char (point-min))
    (let (inside?)
      (while (re-search-forward (rx (not (any "\\")) "\"") pos t)
        (setq inside? (not inside?)))
      inside?)))

(defun bison--in-braced-c-expression? (section-start)
  "Non-nil if the point is within a sexp delimited by braces \({,}\).
SECTION-START is the start of the current section."
  (save-excursion
    (cond
     ((= bison--pre-c-decls-section section-start))

     ((= bison--c-decls-section section-start)
      (-when-let (brace-start (save-excursion (search-backward "%{" nil t)))
        (bison--inside-braces? brace-start)))

     ((= bison--bison-decls-section section-start)
      (let ((brace-end (or (save-excursion (search-backward "%}" nil t))
                           (point-min))))
        (bison--inside-braces? brace-end)))

     ((= bison--grammar-rules-section section-start)
      (-when-let (prod-start (bison--production-start-pos))
        (bison--inside-braces? prod-start)))

     (t
      (= bison--c-code-section section-start)))))

(defun bison--find-open-brace (max-pos)
  "Find an open brace that is not inside a comment or string literal.
MAX-POS sets a limit on the search."
  (cl-loop with n = 1
           while (re-search-backward (rx (not (any "%")) "{") max-pos t n)
           do (goto-char (match-end 0))
           do (if (or (bison--in-c-comment?) (bison--in-string?))
                  (setq n (1+ n))
                (cl-return t))))

(defun bison--inside-braces? (expr-start-pos)
  "Non-nil if point is inside a matched pair of braces.
EXPR-START-POS sets a starting constraint for the search."
  (save-excursion
    (when (bison--find-open-brace (point))
      (-if-let (end-pt (ignore-errors (forward-sexp) (point)))
          (< expr-start-pos end-pt)
        t))))

(defun bison--at-decl-start? (limit)
  "Non-nil if the current line is the beginning of a bison declaration.
Examples include %type, %token, %right.

LIMIT sets an end for the search."
  (s-matches? (eval `(rx bol (or ,@bison--declarations)))
              (buffer-substring (line-beginning-position) limit)))

(defun bison--at-production-start? ()
  "Non-nil if the current line introduces a new production."
  (s-matches? bison--production-re (bison--current-line)))

(defun bison--find-bison-semicolon ()
  "Return the position of next semicolon not within braces.
Return nil of not found."
  (save-excursion
    (when (search-forward ";" nil t)
      (if (not (bison--in-braced-c-expression? (bison--section-start)))
          (point)
        (bison--find-bison-semicolon)))))

(defun bison--inside-production-body? (section-start)
  "Non-nil if the point is inside the body of a production.
SECTION-START is the start of the current section.
Note that this procedure will fail if it is in a production header."
  (save-excursion
    (when (= bison--grammar-rules-section section-start)
      (re-search-backward bison--production-re nil t))))

(defun bison--production-alternative? (section-start)
  "Non-nil if the current line is a rule alternative.
SECTION-START is the start of the current section."
  (save-excursion
    (goto-char (line-beginning-position))
    (when (search-forward "|" (line-end-position) t)
      (not (bison--in-braced-c-expression? section-start)))))

;;;; Indentation

(defun bison--indent-c-sexp (section-start indent-to-col)
  "Perform indentation for a C expression.

SECTION-START is the starting point of the current section.

INDENT-TO-COL is the column to indent the expression to."
  (-if-let (brace-pos
            (re-search-backward (rx (not (any "%")) "{")
                                (line-beginning-position) t))
      (cond
       ((save-excursion
          (goto-char brace-pos)
          (bison--in-braced-c-expression? section-start))
        (c-indent-line))

       ;; Opening-brace is first char, but not in the correct position?
       ((and (= (current-indentation) brace-pos)
             (not (= brace-pos indent-to-col)))
        (back-to-indentation)
        (delete-horizontal-space)
        (indent-to-column indent-to-col)))

    (c-indent-line)))

(defun bison--c-production-indentation-syntax ()
  "Return indentation info for c-indent-line."
  (save-excursion
    (forward-line -1)
    (when (bison--at-production-start?)
      (list
       (cons 'defun-block-intro
             (progn
               (re-search-forward bison--production-re)
               (- (re-search-forward "[^ \t]")
                  1)))))))

(defun bison--search-backward-semicolon ()
  "Search backward for a semicolon."
  (let ((limit (or (bison--production-start-pos)
                   (bison--grammar-section-start-pos))))
    (search-backward ";" limit t)))

(defun bison--grammar-rule-start-col (section-start)
  "Find the start column of the current grammar rule.
Bounded by SECTION-START."
  (save-excursion
    (cond ((and (bison--search-backward-semicolon)
                (bison--in-braced-c-expression? section-start))
           bison-rule-enumeration-column)
          ((bison--production-start-pos)
           bison-rule-enumeration-column)
          (t
           0))))

(defun bison-indent-new-line (&optional c-sexp?)
  "Indent a fresh line of bison code.

If C-SEXP? is non-nil, indent as C code.

Assumes that we are indenting a new line, i.e. at column 0."
  (interactive)
  (let ((sec (bison--section-start)))
    (cond
     ((or c-sexp? (bison--in-braced-c-expression? sec))
      (c-indent-line (and
                      (= sec bison--grammar-rules-section)
                      (bison--c-production-indentation-syntax))))

     ((= sec bison--pre-c-decls-section)
      (c-indent-line))

     ((= sec bison--bison-decls-section)
      (indent-to-column bison-decl-token-column))

     ((= sec bison--grammar-rules-section)
      (indent-to-column (bison--grammar-rule-start-col sec))))))

;; FIXME: Refactor this terrifying monolith.
(defun bison-indent-for-tab-command ()
  "Indent a line of bison code."
  (interactive)
  (let* ((pos (- (point-max) (point)))
         (reset-pt (lambda ()
                     (if (> (- (point-max) pos) (point))
                         (goto-char (- (point-max) pos)))))
         (section (bison--section-start))
         (c-sexp (bison--in-braced-c-expression? section))
         (ws-line (bison--blank-line?)))
    (cond
     (ws-line
      (bison-indent-new-line c-sexp))

     ((= section bison--pre-c-decls-section))

     ((= section bison--c-decls-section)
      (if c-sexp
          (bison--indent-c-sexp section 0)
        (unless (= (current-indentation) 0)
          (back-to-indentation)
          (delete-horizontal-space)
          (funcall reset-pt))))

     ((= section bison--bison-decls-section)
      (let ((opener (bison--at-decl-start? (line-end-position))))
        (cond
         (opener
          (goto-char opener)
          (skip-chars-forward " \t" (line-end-position))
          (cond ((looking-at "{")
                 (save-excursion
                   (when (bison--any-non-spaces-on-line-after-point?)
                     (forward-char 1)
                     (delete-horizontal-space)
                     (newline)
                     (bison-indent-new-line t))))
                (t
                 (let ((complete-type t))
                   (when (looking-at "<")
                     (setq complete-type nil)
                     (cond ((not (= (current-column) bison-decl-type-column))
                            (delete-horizontal-space)
                            (indent-to-column bison-decl-type-column))
                           (t
                            (and (re-search-forward
                                  (concat "<" bison--word-constituent-re "+>")
                                  (line-end-position) t)
                                 (setq complete-type t)))))
                   (and complete-type
                        (skip-chars-forward " \t" (line-end-position))
                        (looking-at
                         (concat "\\(" bison--word-constituent-re "\\|'\\)"))
                        (unless (not (= (current-column) bison-decl-token-column))
                          (delete-horizontal-space)
                          (indent-to-column bison-decl-token-column))))))
          (funcall reset-pt))

         (c-sexp
          (bison--indent-c-sexp section 0))

         (t
          (back-to-indentation)
          ;; only tab in names, leave comments alone
          (cond
           ;; put word-constiuents in bison-decl-token-column
           ((looking-at bison--word-constituent-re)
            (unless (= (current-column) bison-decl-token-column)
              (delete-horizontal-space)
              (indent-to-column bison-decl-token-column)))
           ;; Put or keep close-brace in the 0 column
           ((looking-at "}")
            (unless (zerop (current-column))
              (delete-horizontal-space)))
           ;; Leave comments alone
           ((looking-at (regexp-quote comment-start)) nil))
          (funcall reset-pt)))))

     ((= section bison--grammar-rules-section)
      (cond
       ((bison--at-production-start?)
        (beginning-of-line)
        (re-search-forward bison--production-re)
        (when (bison--any-non-spaces-on-line-after-point?)
          (cond ((> (current-column) bison-rule-enumeration-column)
                 (delete-horizontal-space)
                 (newline)
                 (indent-to-column bison-rule-enumeration-column))
                (t
                 (save-excursion
                   (re-search-forward bison--word-constituent-re) ;; SIGERR
                   (let ((col (current-column)))
                     (cond ((> col (+ 1 bison-rule-enumeration-column))
                            (forward-char -1)
                            (delete-horizontal-space)
                            (indent-to-column bison-rule-enumeration-column))
                           ((< col (+ 1 bison-rule-enumeration-column))
                            (forward-char -1)
                            (indent-to-column
                             bison-rule-enumeration-column))))))))
        (funcall reset-pt))

       ((bison--production-alternative? section)

        ;; Move point to "|"
        (back-to-indentation)

        (unless (= (current-column) bison-rule-separator-column)
          (delete-horizontal-space)
          (indent-to-column bison-rule-separator-column))
        (forward-char 1)
        (when (bison--any-non-spaces-on-line-after-point?)
          (save-excursion
            (re-search-forward bison--word-constituent-re)
            (let ((col (current-column)))
              (cond ((> col (+ 1 bison-rule-enumeration-column))
                     (forward-char -1)
                     (delete-horizontal-space)
                     (indent-to-column bison-rule-enumeration-column))
                    ((< col (+ 1 bison-rule-enumeration-column))
                     (forward-char -1)
                     (indent-to-column
                      bison-rule-enumeration-column))))))
        (funcall reset-pt))

       (c-sexp
        (bison--indent-c-sexp section bison-rule-enumeration-column)
        (funcall reset-pt))

       ((bison--inside-production-body? section)
        (back-to-indentation)
        (unless (= (current-column) bison-rule-enumeration-column)
          (delete-horizontal-space)
          (indent-to-column
           bison-rule-enumeration-column))
        (funcall reset-pt))

       (t
        (let ((cur-ind (current-indentation)))
          (when (eq (save-excursion
                      (search-backward "}"
                                       (line-beginning-position) t))
                    cur-ind)
            (unless (= cur-ind bison-rule-enumeration-column)
              (back-to-indentation)
              (delete-horizontal-space)
              (indent-to-column bison-rule-enumeration-column)
              (funcall reset-pt)))))))

     ((= section bison--c-code-section)
      (c-indent-line)))))

;;;; Electric Commands

(defun bison-electric-colon ()
  "Insert a context-sensitive colon character.

If at a production, insert a semicolon on the next line at
`bison-rule-seperator-column', then move to
`bison-rule-enumerator-column'.

Otherwise insert a single colon."
  (interactive)
  (insert ":")
  (unless bison-disable-electric-keys?
    (when (and (= bison--grammar-rules-section (bison--section-start))
               (bison--after-production?)
               (not (bison--at-unfinished-production?)))
      ;; insert closing semicolon
      (save-excursion
        (newline)
        (indent-to-column bison-rule-separator-column)
        (insert ";"))
      ;; remove leading whitespace
      (save-excursion
        (when (re-search-backward "\\s " (line-beginning-position) t)
          (delete-horizontal-space)))

      (unless (< (current-column) bison-rule-enumeration-column)
        (newline))

      (indent-to-column bison-rule-enumeration-column))))

(defun bison-electric-pipe ()
  "Insert a pipe character.
If the pipe was used as a rule separator, align the pipe accordingly."
  (interactive)
  (if bison-disable-electric-keys?
      (insert "|")

    (when (and (= bison--grammar-rules-section (bison--section-start))
               (bison--blank-line?))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to-column bison-rule-separator-column)
      (insert "|")
      (indent-to-column bison-rule-enumeration-column))))

(defun bison-electric-open-brace ()
  "Insert an opening curly brace and apply formatting."
  (interactive)
  (unless bison-disable-electric-keys?
    (let ((section (bison--section-start)))
      (cond
       ((and (= section bison--grammar-rules-section)
             (not (bison--in-braced-c-expression? section))
             (not (bison--any-non-spaces-on-line-before-point?)))
        (unless (= (current-column) bison-rule-enumeration-column)
          (delete-horizontal-space)
          (indent-to-column bison-rule-enumeration-column)))

       ((and (= section bison--bison-decls-section)
             (not (bison--in-braced-c-expression? section))
             (not (bison--any-non-spaces-on-line-before-point?)))

        (unless (zerop (current-column))
          (delete-horizontal-space)
          (indent-to-column 0))))))

  (insert "{"))

(defun bison-electric-close-brace ()
  "Insert a closing curly brace and apply formatting."
  (interactive)
  (insert "}")
  (unless bison-disable-electric-keys?
    (when (search-backward "%}" (- (point) 2) t)
      (if (/= (bison--section-start) bison--c-decls-section)
          (forward-char 1)
        ;; Format "%}"
        (delete-horizontal-space)
        (forward-char 2)))))

(defun bison-electric-percent ()
  "Insert a % character.
If this begins a declaration, move it to the start column."
  (interactive)
  (unless bison-disable-electric-keys?
    (when (= (bison--section-start) bison--bison-decls-section)
      (unless (or (bison--in-braced-c-expression? (bison--section-start))
                  (bison--any-non-spaces-on-line-before-point?)
                  (zerop (current-column)))
        (delete-horizontal-space))))

  (insert "%"))

(defun bison-electric-less-than ()
  "Insert a < character.
If it begins a type declaration, indent to `bison-decl-type-column'."
  (interactive)
  (unless bison-disable-electric-keys?
    (when (and (= (bison--section-start) bison--bison-decls-section)
               (bison--at-decl-start? (point)))
      (delete-horizontal-space)
      (indent-to-column bison-decl-type-column)))

  (insert "<"))

(defun bison-electric-greater-than ()
  "Insert a > character.
If it ends a type declaration, indent to `bison-decl-token-column'."
  (interactive)
  (insert ">")
  (unless bison-disable-electric-keys?
    (let ((start-pos (point)))
      (when (and (= (bison--section-start) bison--bison-decls-section)
                 (bison--at-decl-start? (point))
                 (search-backward "<" (line-beginning-position) t)
                 (re-search-forward (concat "<" bison--word-constituent-re "+>")
                                    start-pos t))
        (unless (bison--any-non-spaces-on-line-after-point?)
          (delete-horizontal-space)
          (indent-to-column bison-decl-token-column))))))

;;;; Mode Definition

(make-variable-buffer-local 'c-offsets-alist)

;;;###autoload
(defvar bison-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd ":") 'bison-electric-colon)
    (define-key km (kbd "|") 'bison-electric-pipe)
    (define-key km (kbd "{") 'bison-electric-open-brace)
    (define-key km (kbd "}") 'bison-electric-close-brace)
    (define-key km (kbd "%") 'bison-electric-percent)
    (define-key km (kbd "<") 'bison-electric-less-than)
    (define-key km (kbd ">") 'bison-electric-greater-than)
    (define-key km (kbd "TAB") 'bison-indent-for-tab-command)
    km)
  "Keymap for `bison-mode'.")

;;;###autoload
(define-derived-mode bison-mode c-mode "Bison"
  "Major mode for editing bison/yacc files.

\\{bison-mode-map}"
  (setq-local c-basic-offset 4)
  (c-set-offset 'knr-argdecl-intro 0)

  ;; Disable disruptive C minor modes.
  (c-toggle-auto-hungry-state -1)
  (c-toggle-auto-newline -1)
  (c-toggle-hungry-state -1)

  ;; Configure indentation and comments.
  (setq-local indent-line-function 'bison-indent-new-line)
  (setq-local comment-start "/*")
  (setq-local comment-end "*/")

  ;; Configure font-lock.
  (setq-local font-lock-keywords nil)
  (setq-local font-lock-defaults '((bison-font-lock-keywords
                                    bison-font-lock-keywords-1
                                    bison-font-lock-keywords-2)
                                   nil nil nil)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.y$" . bison-mode))

(provide 'bison-mode)

;;; bison-mode.el ends here
