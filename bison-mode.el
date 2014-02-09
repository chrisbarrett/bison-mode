;;; bison-mode.el --- Major mode for editing bison/yacc files

;; Copyright (C) 1998 Eric Beuscher

;; Author:   Eric Beuscher <beuscher@eecs.tulane.edu>
;; Created:  2 Feb 1998
;; Version:  0.2
;; Package-Requires: ((cl-lib "0.2"))
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

(require 'cl-lib)
(require 'cc-mode)

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

(defvar bison--declarers
  '("%union" "%token" "%type" "%left" "%right" "%nonassoc")
  "List of commands which can declare a token or state type.")

(defvar bison--word-constituent-re "\\(\\sw\\|_\\)")

(defvar bison--production-re
  (concat "^" bison--word-constituent-re "+:"))

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
    (cons (eval `(rx bol (group-n 1 (or ,@bison--declarers))))
          '(1 font-lock-keyword-face)))
   bison-font-lock-keywords-1)
  "Gaudy highlighting for Bison mode.")

(defvar bison-font-lock-keywords bison-font-lock-keywords-2
  "Default expressions to highlight in Bison mode.")

;;;; Utility Functions

(defun same-line-p (pt1 pt2 &optional bol eol)
  "Non-nil if PT1 and PT2 are on the same line.

Optional args BOL and EOL are buffer positions.  The function will
return nil if either PT1 or PT2 are outside these bounds."
  (let ((bol (or bol (save-excursion (beginning-of-line) (point))))
        (eol (or eol (save-excursion (end-of-line) (point)))))
    (and (<= bol pt1) (<= bol pt2)
         (>= eol pt1) (>= eol pt2))))

(defun white-space-separation (pt1 pt2)
  "Non-nil if there is nothing but whitespace between PT1 and PT2 (exclusive)."
  (save-excursion
    (goto-char (+ pt1 1))
    (not (re-search-forward "[^ \t\n]" pt2 t))))

(defun previous-white-space-p ()
  "Non-nil if there is whitespace between the beginning of the line and point."
  (save-excursion
    (let ((current-point (point)))
      (beginning-of-line)
      (if (re-search-forward "\\s " current-point t)
          t
        nil))))

(defun previous-non-ws-p ()
  "Non-nil if there are non-whitespace chars between bol and point."
  (save-excursion
    (let ((current-point (point)))
      (beginning-of-line)
      (re-search-forward "[^ \t]" current-point t)
      )))

(defun following-non-ws-p ()
  "Non-nil if there are non-whitespace characters on this line."
  (save-excursion
    (let ((current-point (point)))
      (end-of-line)
      (re-search-backward "[^ \t]+" current-point t)
      )))

(defun line-of-whitespace-p ()
  "Non-nil if the current line is empty or entirely whitespace."
  (save-excursion
    (let ((eol (progn (end-of-line) (point))))
      (beginning-of-line)	;; should already be there anyway
      (not (re-search-forward "[^ \t\n]" eol t)))))

;;;; Mode Definition

(make-variable-buffer-local 'c-offsets-alist)

;;;###autoload
(defvar bison-mode-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km c-mode-map)
    (define-key km (kbd ":") 'bison-electric-colon)
    (define-key km (kbd "|") 'bison-electric-pipe)
    (define-key km (kbd "{") 'bison-electric-open-brace)
    (define-key km (kbd "}") 'bison-electric-close-brace)
    (define-key km (kbd "%") 'bison-electric-percent)
    (define-key km (kbd "<") 'bison-electric-less-than)
    (define-key km (kbd ">") 'bison-electric-greater-than)
    (define-key km (kbd "TAB") 'bison-indent-line)
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

(defun bison--production-p ()
  "Non-nil if point is immediately after a production."
  (save-excursion
    (let ((current-point (point)))
      (beginning-of-line)
      (let ((position (re-search-forward
                       bison--production-re current-point t)))
        (and position
             (not (previous-white-space-p))
             (= position current-point))))))

(defun bison--find-production-opener ()
  "Try to move to the start of the production before point.
Return the buffer position on success."
  (re-search-backward bison--production-re nil t))

(defun bison--find-next-production ()

  "Try to move to the start of next production after point.
Return the buffer position on success."
  (save-excursion
    (if (re-search-forward bison--production-re nil t)
        (progn
          (beginning-of-line)
          (point))
      nil)))

(defun bison--find-grammar-end ()
  "Return the position of the end of the grammar rules.
Return nil if the end cannot be found."
  (save-excursion
    (if (re-search-forward
         (concat "^" bison--grammar-rules-section-delimeter)
         nil t)
        (progn
          (beginning-of-line)
          (point))
      nil)))

(defun bison--find-grammar-begin ()
  "Return the position of the beginning of the grammar rules.
Return nil if the start cannot be found."
  (save-excursion
    (if (re-search-backward
         (concat "^" bison--grammar-rules-section-delimeter)
         nil t)
        (point)
      nil)))

(defun bison--within-started-production-p ()
  "Non-nil if point is at a production."
  ;; Point is within a production if there is some non-whitespace text before:
  ;;
  ;; - the beginnings of another production, or
  ;;
  ;; - the end of the grammar rules
  (save-excursion
    (let ((bound (cond ((bison--find-next-production))
                       ((bison--find-grammar-end))
                       (t nil))))
      (if bound
          (let ((sval (re-search-forward
                       (concat "\\(\\s \\|" ;; whitespace or
                               ;; comments
                               (regexp-quote comment-start)
                               "\\(.\\|\n\\)*" ;; comment body
                               (regexp-quote comment-end)
                               "\\)+")	;; end or
                       bound t)))
            (if sval
                (not (= sval bound))
              nil))
        nil))))

(defun bison--within-some-sexp-p (start-regexp end-regexp)
  "Non-nil if point is within a sexp between START-REGEXP and END-REGEXP."
  (save-excursion
    (let ((current-point (point)))
      (if (re-search-backward start-regexp nil t) ;; find nearest start-regexp
          ;; look for end-regexp, if found, then not within sexp
          (progn
            (goto-char (match-end 0))
            (not (re-search-forward end-regexp current-point t)))))))

(defun bison--within-c-comment-p ()
  "Non-nil if point is inside a C comment delimited by \"/*\" \"*/\"."
  (bison--within-some-sexp-p (regexp-quote comment-start)
                             (regexp-quote comment-end)))

(defun bison--within-string-p (&optional point)
  "Non-nil if POINT is inside a string."
  ;; Start from the beginning of the buffer and toggle state as un-escaped \"'s
  ;; are found.
  (let ((point (or point (point)))
        (in-p nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[^\\]\"" point t)
        (setq in-p (not in-p)))

      in-p)))

(defun bison--within-braced-c-expression-p (section)
  "Non-nil if the point is within a sexp delimited by braces \({,}\)."
  (save-excursion
    (let ((low-pt (point)))
      (cond ((= section bison--pre-c-decls-section) nil)
            ((= section bison--c-decls-section)
             (let ((opener (save-excursion (search-backward "%{"))))
               (bison--inside-braces? opener low-pt)))
            ((= section bison--bison-decls-section)
             (let ((opener (save-excursion
                             (or (search-backward "%}" nil t)
                                 (point-min)))))
               (bison--inside-braces? opener low-pt)))
            ((= section bison--grammar-rules-section)
             (let ((opener (save-excursion (bison--find-production-opener))))
               (if opener
                   (bison--inside-braces? opener low-pt)
                 nil)))
            ((= section bison--c-code-section)
             t)))))

(defun bison--inside-braces? (high-pt low-pt)
  "Non-nil if HIGH-PT and LOW-PT are inside a matched pair of braces."
  ;; HIGH-PT goes toward point-min, LOW-PT goes toward point-max.
  ;; save-excursion is done higher up, so I don't concern myself here.
  (let ((pt (point)))
    (let ((success nil) (count 1) (done nil))
      ;; loop until open brace found, that is not in comment or string literal
      (while (and (not done)
                  (re-search-backward "[^%]{" high-pt t count)) ;find nearest
                                        ;starter
        (goto-char (match-end 0))
        (if (or (bison--within-c-comment-p)
                (bison--within-string-p))

            (setq count (+ count 1))
          (progn
            (setq success t)
            (setq done t))))

      (if success
          (let ((end-pt
                 (condition-case nil
                     (progn (forward-sexp) (point))
                   (error nil))))
            (if end-pt
                (if (> end-pt low-pt)
                    t			; then in braced-c-exp
                  nil)
              t))			; if no sexp close brace, then w/in
        nil))))

(defun bison--bison-decl-start-p (bol eol)
  "Non-nil if the current line is the beginning of a bison declaration.
Examples include %type, %token, %right.

BOL and EOL constrain the search."
  (save-excursion
    (goto-char bol)
    (re-search-forward (eval `(rx bol (or ,@bison--declarers)))
                       eol t)))

(defun bison--production-opener-p (bol eol)
  "Non-nil if the current line introduces a new production.
BOL and EOL constrain the search."
  (save-excursion
    (goto-char bol)
    (re-search-forward bison--production-re eol t)))

(defun bison--find-bison-semicolon ()
  "Return the position of next semicolon not within braces.
Return nil of not found."
  (save-excursion
    (if (search-forward ";" nil t)
        (if (not (bison--within-braced-c-expression-p (bison--section-start)))
            (point)
          (bison--find-bison-semicolon))
      nil)))

(defun bison--inside-production-body-p (section)
  "Non-nil if the point is inside the body of a production.
Note that this procedure will fail if it is in a production header."
  (save-excursion
    (if (= section bison--grammar-rules-section)
        (let ((current-point (point)))
          (if (re-search-backward bison--production-re nil t)
              t
            nil))
      nil)))

(defun bison--production-alternative-p (bol eol section)
  "Non-nil if the current line contains a \"|\" designating a rule alternative.
BOL and EOL constrain the search."
  (save-excursion
    (goto-char bol)
    (if (search-forward "|" eol t)
        (not (bison--within-braced-c-expression-p section))
      nil)))

;;;; Indentation

(defun bison--handle-indent-c-sexp (section indent-column bol)
  (let* ((o-brace (re-search-backward "[^%]{" bol t))
         )
    (if o-brace
        (if (save-excursion
              (goto-char o-brace)
              (bison--within-braced-c-expression-p section))
            (c-indent-line)
          (if (= (current-indentation) o-brace)	;; if o-brace is first char
              (if (not (= o-brace indent-column)) ;; but not in right spot
                  (progn
                    (back-to-indentation)
                    (delete-horizontal-space)
                    (indent-to-column indent-column))
                ;; else all is good
                )
            ;; else, non-ws before o-brace, leave it alone
            ))
      (c-indent-line))))

(defun bison-indent-new-line (&optional c-sexp)
  "Indent a fresh line of bison code.

Assumes that we are indenting a new line, i.e. at column 0."
  (interactive)
  (let ((section (bison--section-start)))
    (cond
     ((or c-sexp (bison--within-braced-c-expression-p section))
      (cond
       ((= section bison--grammar-rules-section)
        (c-indent-line
         (save-excursion
           (forward-line -1)
           (let ((bol (save-excursion (beginning-of-line) (point)))
                 (eol (save-excursion (end-of-line) (point))))
             (if (bison--production-opener-p bol eol)
                 (list
                  (cons 'defun-block-intro
                        (progn
                          (re-search-forward bison--production-re) ; SIGERR
                          (- (re-search-forward "[^ \t]")          ; SIGERR
                             1))))
               nil)))))
       (t (c-indent-line))))
     ((= section bison--pre-c-decls-section)
      (c-indent-line))
     ((= section bison--bison-decls-section)
      (indent-to-column bison-decl-token-column))
     ((= section bison--grammar-rules-section)
      (indent-to-column
       (save-excursion
         (let* ((bound (or (save-excursion (bison--find-production-opener))
                           (bison--find-grammar-begin)))
                (prev-semi (search-backward ";" bound t))
                )
           (if prev-semi
               (if (bison--within-braced-c-expression-p section) ; CRACK
                   bison-rule-enumeration-column
                 0)
             (if (save-excursion (bison--find-production-opener))
                 bison-rule-enumeration-column
               0)))))))))

(defun bison-indent-line ()
  "Indent a line of bison code."
  (interactive)
  (let* ((pos (- (point-max) (point)))
         (reset-pt (lambda ()
                     (if (> (- (point-max) pos) (point))
                         (goto-char (- (point-max) pos)))))
         (bol (save-excursion (beginning-of-line) (point)))
         (eol (save-excursion (end-of-line) (point)))
         )
    (let* ((section (bison--section-start))
           (c-sexp (bison--within-braced-c-expression-p section))
           (ws-line (line-of-whitespace-p))
           )
      (cond
       ;; if you are a line of whitespace, let indent-new-line take care of it
       (ws-line
        (bison-indent-new-line c-sexp))

       ((= section bison--pre-c-decls-section)
        ;; leave things alone
        )

       ((= section bison--c-decls-section)
        (if c-sexp
            (bison--handle-indent-c-sexp section 0 bol)
          (unless (= (current-indentation) 0)
            (back-to-indentation)
            (delete-horizontal-space)
            (funcall reset-pt))))

       ((= section bison--bison-decls-section)
        (let ((opener (bison--bison-decl-start-p bol eol)))
          (cond
           (opener
            (goto-char opener)
            (skip-chars-forward " \t" eol)
            (if (looking-at "{")
                (save-excursion
                  (if (following-non-ws-p)
                      (progn
                        (forward-char 1)
                        (delete-horizontal-space)
                        (newline)
                        (bison-indent-new-line t))))
              (let ((complete-type t))
                (if (looking-at "<")
                    (progn
                      (setq complete-type nil)
                      (if (not (= (current-column) bison-decl-type-column))
                          (progn
                            (delete-horizontal-space)
                            (indent-to-column bison-decl-type-column))
                        (and (re-search-forward
                              (concat "<" bison--word-constituent-re "+>")
                              eol t)
                             (setq complete-type t)))))
                (and complete-type
                     (skip-chars-forward " \t" eol)
                     (looking-at
                      (concat "\\(" bison--word-constituent-re "\\|'\\)"))
                     (if (not (= (current-column) bison-decl-token-column))
                         (progn
                           (delete-horizontal-space)
                           (indent-to-column bison-decl-token-column))))))
            (funcall reset-pt))
           (c-sexp
            (bison--handle-indent-c-sexp section 0 bol))
           (t
            (back-to-indentation)
            ;; only tab in names, leave comments alone
            (cond (;; put word-constiuents in bison-decl-token-column
                   (looking-at bison--word-constituent-re)
                   (if (not (= (current-column) bison-decl-token-column))
                       (progn
                         (delete-horizontal-space)
                         (indent-to-column bison-decl-token-column))))
                  ;; put/keep close-brace in the 0 column
                  ((looking-at "}")
                   (if (not (= (current-column) 0))
                       (delete-horizontal-space)))
                  ;; leave comments alone
                  ((looking-at (regexp-quote comment-start)) nil)
                  ;; else do nothing
                  )
            (funcall reset-pt)))))
       ((= section bison--grammar-rules-section)
        (cond
         ((bison--production-opener-p bol eol)
          (beginning-of-line)
          (re-search-forward bison--production-re);; SIGERR
          (if (following-non-ws-p)
              (if (> (current-column) bison-rule-enumeration-column)
                  (progn
                    (delete-horizontal-space)
                    (newline)
                    (indent-to-column bison-rule-enumeration-column))
                (save-excursion
                  (re-search-forward bison--word-constituent-re);; SIGERR
                  (let ((col (current-column)))
                    (cond ((> col (+ 1 bison-rule-enumeration-column))
                           (forward-char -1)
                           (delete-horizontal-space)
                           (indent-to-column bison-rule-enumeration-column))
                          ((< col (+ 1 bison-rule-enumeration-column))
                           (forward-char -1)
                           (indent-to-column
                            bison-rule-enumeration-column)))))))
          (funcall reset-pt))
         ((bison--production-alternative-p bol eol section)
          (back-to-indentation);; should put point on "|"
          (if (not (= (current-column) bison-rule-separator-column))
              (progn
                (delete-horizontal-space)
                (indent-to-column bison-rule-separator-column)))
          (forward-char 1)
          (if (following-non-ws-p)
              (save-excursion
                (re-search-forward bison--word-constituent-re);; SIGERR
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
          (bison--handle-indent-c-sexp
           section bison-rule-enumeration-column bol)
          (funcall reset-pt))
         ((bison--inside-production-body-p section)
          (back-to-indentation)
          (if (not (= (current-column) bison-rule-enumeration-column))
              (progn
                (delete-horizontal-space)
                (indent-to-column
                 bison-rule-enumeration-column)))
          (funcall reset-pt))
         (t
          (let ((cur-ind (current-indentation)))
            (if (eq (save-excursion (search-backward "}" bol t))
                    cur-ind)
                (if (not (= cur-ind bison-rule-enumeration-column))
                    (progn
                      (back-to-indentation)
                      (delete-horizontal-space)
                      (indent-to-column bison-rule-enumeration-column)
                      (funcall reset-pt)))
              ;; else leave alone
              )))))
       ((= section bison--c-code-section)
        (c-indent-line))
       ))))

;;;; Electric Commands

(defun bison-electric-colon ()
  "Insert a context-sensitive colon character.

If at a production, insert a semicolon on the next line at
`bison-rule-seperator-column', then move to
`bison-rule-enumerator-column'.

Otherwise insert a single colon."
  (interactive)
  (insert ":")
  (if (not bison-disable-electric-keys?)
      (if (and (= bison--grammar-rules-section (bison--section-start))
               (bison--production-p)
               (not (bison--within-started-production-p)))
          (progn
            (save-excursion		; put in a closing semicolon
              (newline)
              (indent-to-column bison-rule-separator-column)
              (insert ";"))
            (save-excursion		; remove opening whitespace
              (if (re-search-backward
                   "\\s "
                   (save-excursion (beginning-of-line) (point))
                   t)
                  (delete-horizontal-space)))
            (if (not (< (current-column) bison-rule-enumeration-column))
                (newline))
            (indent-to-column bison-rule-enumeration-column)))
    ))

(defun bison-electric-pipe ()
  "Insert a pipe character.
If the pipe was used as a rule separator, align the pipe accordingly."
  (interactive)
  (if (and (not bison-disable-electric-keys?)
           (= bison--grammar-rules-section (bison--section-start))
           (line-of-whitespace-p)
           )
      (progn
        (beginning-of-line)
        (delete-horizontal-space)
        (indent-to-column bison-rule-separator-column)
        (insert "|")
        (indent-to-column bison-rule-enumeration-column))
    (insert "|")))

(defun bison-electric-open-brace ()
  "Insert an opening curly brace and apply formatting."
  (interactive)
  (if (not bison-disable-electric-keys?)
      (let ((section (bison--section-start)))
        (cond ((and (= section bison--grammar-rules-section)
                    (not (bison--within-braced-c-expression-p section))
                    (not (previous-non-ws-p)))
               (if (not (= (current-column) bison-rule-enumeration-column))
                   (progn
                     (delete-horizontal-space)
                     (indent-to-column bison-rule-enumeration-column))))
              ((and (= section bison--bison-decls-section)
                    (not (bison--within-braced-c-expression-p section))
                    (not (previous-non-ws-p)))
               (if (not (= (current-column) 0))
                   (progn
                     (delete-horizontal-space)
                     (indent-to-column 0)))))))
  (insert "{"))

(defun bison-electric-close-brace ()
  "Insert an closing curly brace and apply formatting."
  (interactive)
  (insert "}")
  (if (not bison-disable-electric-keys?)
      (cond ((search-backward "%}" (- (point) 2) t)
             (if (= (bison--section-start) bison--c-decls-section)
                 (progn
                   (delete-horizontal-space)
                   (forward-char 2))	; for "%}"
               (forward-char 1))))))

(defun bison-electric-percent ()
  "Insert a % character.
If this begins a declaration, move it to the start column."
  (interactive)
  (if (not bison-disable-electric-keys?)
      (let ((section (bison--section-start)))
        (if (and (= section bison--bison-decls-section)
                 (not (bison--within-braced-c-expression-p section))
                 (not (previous-non-ws-p))
                 (not (= (current-column) 0)))
            (delete-horizontal-space))))
  (insert "%"))

(defun bison-electric-less-than ()
  "Insert a < character.
If it begins a type declaration, indent to `bison-decl-type-column'."
  (interactive)
  (if (not bison-disable-electric-keys?)
      (if (and (= (bison--section-start) bison--bison-decls-section)
               (bison--bison-decl-start-p
                (save-excursion (beginning-of-line) (point))
                (point)))
          (progn
            (delete-horizontal-space)
            (indent-to-column bison-decl-type-column))))
  (insert "<"))

(defun bison-electric-greater-than ()
  "Insert a > character.
If it ends a type declaration, indent to `bison-decl-token-column'."
  (interactive)
  (insert ">")
  (if (not bison-disable-electric-keys?)
      (let ((current-pt (point))
            (bol (save-excursion (beginning-of-line) (point))))
        (if (and (= (bison--section-p) bison--bison-decls-section)
                 (bison--bison-decl-start-p bol (point)))
            (if (search-backward "<" bol t)
                (if (re-search-forward
                     (concat "<" bison--word-constituent-re "+>")
                     current-pt t)
                    (if (not (following-non-ws-p))
                        (progn
                          (delete-horizontal-space)
                          (indent-to-column bison-decl-token-column)))))))))

(provide 'bison-mode)

;;; bison-mode.el ends here
