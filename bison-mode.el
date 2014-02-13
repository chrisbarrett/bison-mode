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
;; package perform different actions depending on the current section.

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

(autoload 'thing-at-point-looking-at "thingatpt")

(defgroup bison-mode nil
  "Major mode for editing bison and yacc files"
  :group 'languages
  :prefix "bison-")

(defcustom bison-rule-case-column 4
  "Column for rule and production separators \"|\" and \";\"."
  :group 'bison-mode
  :type 'integer)

(defcustom bison-decl-c-column 8
  "Column at which lines in the C declarations block should be indented."
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

;;;; Internal Variables

(defvar bison--declarations
  '("%define" "%union" "%token" "%type" "%left" "%right" "%nonassoc")
  "List of commands which can declare a token or state type.")

(defvar bison-font-lock-keywords
  (let ((kws2 (eval `(rx bol (group-n 1 (or ,@bison--declarations))))))
    (cons (cons kws2 '(1 font-lock-keyword-face))
          c-font-lock-keywords))
  "Keywords to highlight in Bison mode.  This is a superset of C keywords.")

;;;; Indentation

(defun bison--in-string? ()
  "Non-nil if point is inside a string according to font locking."
  (ignore-errors
    (equal 'font-lock-string-face (get-char-property (1- (point)) 'face))))

(defun bison--in-comment? ()
  "Non-nil if point is inside a comment according to font locking."
  (ignore-errors
    (equal 'font-lock-comment-face (get-char-property (1- (point)) 'face))))

(defun bison--backwards-up-to-top-curly-braces ()

  "Move to the top curly brace containing point."
  (let (pos)
    (while (ignore-errors (goto-char (c-up-list-backward)) t)
      (when (thing-at-point-looking-at "{")
        (setq pos (point))))
    pos))

(defun bison--in-c-block? ()
  "Non-nil if point is inside a c block in a production."
  (save-excursion (bison--backwards-up-to-top-curly-braces)))

(defun bison--in-c-declarations-section? ()
  "Non-nil if point is inside the C declarations section of a bison file."
  (save-excursion
    (cl-loop while (ignore-errors (goto-char (c-up-list-backward)) t)
             do (when (thing-at-point-looking-at "%{") (cl-return t)))))

(defun bison--current-line ()
  "Return the current line as a string."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun bison--in-c-section? ()
  "Non-nil if point is in the C section of a bison file.
This is the final section, after the bison grammar declarations."
  (save-excursion (search-backward "%%" nil t 2)))

(defun bison--at-production-header? ()
  (s-matches? (rx bol (* space) (* word) (* space) ":")
              (bison--current-line)))

(defun bison--at-first-production-case? ()
  (save-excursion
    (unless (bison--at-production-case?)
      (forward-line -1)
      (bison--at-production-header?))))

(defun bison--at-production-case? ()
  (s-matches? (rx bol (* space) "|") (bison--current-line)))

(defun bison--at-production-terminating-semicolon? ()
  (s-matches? (rx bol (* space) ";") (bison--current-line)))

(defun bison--at-c-declarations-braces? ()
  (s-matches? (rx bol (* space) (or "%{" "%}")) (bison--current-line)))

(defun bison-indent-line ()
  "Indent the current line, using C or bison formatting styles as appropriate."
  (interactive)
  (save-excursion
    (cond
     ((or (bison--in-comment?) (bison--in-string?))
      'noindent)

     ((bison--at-c-declarations-braces?)
      (goto-char (line-beginning-position))
      (delete-horizontal-space))

     ((bison--in-c-declarations-section?)
      (goto-char (line-beginning-position))
      (delete-horizontal-space)
      (indent-to bison-decl-c-column))

     ((bison--in-c-section?)
      (c-indent-line nil t))

     ((bison--in-c-block?)
      (c-indent-line nil t))

     ((bison--at-production-header?)
      (goto-char (line-beginning-position))
      (delete-horizontal-space)
      (indent-to 0))

     ((bison--at-first-production-case?)
      (goto-char (line-beginning-position))
      (delete-horizontal-space)
      (indent-to (+ 2 bison-rule-case-column)))

     ((or (bison--at-production-case?)
          (bison--at-production-terminating-semicolon?))
      (goto-char (line-beginning-position))
      (delete-horizontal-space)
      (indent-to bison-rule-case-column)))))

;;;; Mode Definition

(make-variable-buffer-local 'c-offsets-alist)

;;;###autoload
(defvar bison-mode-map
  (let ((km (make-sparse-keymap)))
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

  ;; Configure indentation and comments.
  (setq-local indent-line-function 'bison-indent-line)
  (setq-local comment-start "/*")
  (setq-local comment-end "*/")
  (setq-local c-electric-flag nil)

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
