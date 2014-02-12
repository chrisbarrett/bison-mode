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

;;;; Internal Variables

(defvar bison--declarations
  '("%define" "%union" "%token" "%type" "%left" "%right" "%nonassoc")
  "List of commands which can declare a token or state type.")

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

;;;; Section Parsers

;;;; Syntax Parsers

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
  (c-toggle-auto-newline -1)
  (c-toggle-hungry-state -1)

  ;; Configure indentation and comments.
  (setq-local indent-line-function 'bison-indent-new-line)
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
