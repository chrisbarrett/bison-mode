;;; bison-mode.el --- Major mode for editing bison files

;; Copyright (C) 2014 Chris Barrett

;; Author:   Chris Barrett
;; Version:  0.2
;; Package-Requires: ((cl-lib "0.2") (dash "2.5.0") (s "1.7.0"))
;; Keywords: bison-mode

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
;;
;; A bison file is divided into a number of sections. Many commands in this
;; package perform different actions depending on the current section.
;;
;; bison-mode's indentation commands are context-sensitive. If point is in C
;; code the mode falls back the normal indentation behaviour for C.
;;
;; bison-mode also has a command, `bison-format-buffer', which will reindent the
;; entire buffer and line up C blocks in an attractive way.
;;
;;; File History:
;;
;; This mode is based of bison-mode by Eric Beuscher <beuscher@eecs.tulane.edu>.
;; The original version is available here:
;;
;;   http://ftp.sunet.se/pub/gnu/emacs-lisp/incoming/bison-mode.el
;;
;; It has since been entirely rewritten by Chris Barrett
;; <chris.d.barrett@me.com>.

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

(defcustom bison-production-case-column 4
  "Column for rule and production separators \"|\" and \";\"."
  :group 'bison-mode
  :type 'integer)

(defcustom bison-decl-c-column 4
  "Column at which lines in the C declarations block should be indented."
  :group 'bison-mode
  :type 'integer)

(defcustom bison-minimum-c-block-column 30
  "The minimum column to use when aligning C code blocks in productions."
  :group 'bison-mode
  :type 'integer)

(defcustom bison-apply-inner-padding-to-c-blocks? t
  "If non-nil, add a space of inner padding to C code blocks in productions."
  :group 'bison-mode
  :type 'boolean)

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
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

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

(defun bison--at-end-of-production? ()
  "Non-nil if point is at the end of the production."
  (s-matches? (rx ";" (* space) eol) (bison--current-line)))

(defun bison--at-c-declarations-braces? ()
  (s-matches? (rx bol (* space) (or "%{" "%}")) (bison--current-line)))

(defun bison--in-multiline-c-block? ()
  "Non-nil if point is in a multi-line C block in a production."
  (when (bison--in-c-block?)
    (save-excursion
      (goto-char (line-beginning-position))
      ;; Presume point is still in the same C block.
      (bison--in-c-block?))))

(defun bison-indent-line ()
  "Indent the current line, using C or bison formatting styles as appropriate."
  (interactive)
  (let ((s (bison--current-line)))

    ;; Perform indentation.
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

       ;; Indent C code blocks using C indentation.

       ((bison--in-c-section?)
        (c-indent-line nil t))

       ;; If multi-line, do c indent, otherwise indent as production.
       ((bison--in-multiline-c-block?)
        (c-indent-line nil t))
       ((bison--in-c-block?)
        (goto-char (line-beginning-position))
        (delete-horizontal-space)
        (indent-to (+ 2 bison-production-case-column)))

       ;; Indent productions.

       ((bison--at-production-header?)
        (goto-char (line-beginning-position))
        (delete-horizontal-space)
        (indent-to 0))

       ((bison--at-first-production-case?)
        (goto-char (line-beginning-position))
        (delete-horizontal-space)
        (indent-to (+ 2 bison-production-case-column)))

       ((or (bison--at-production-case?)
            (bison--at-end-of-production?))
        (goto-char (line-beginning-position))
        (delete-horizontal-space)
        (indent-to bison-production-case-column))))

    ;; Clear dirty state if line was not actually modified.
    (set-buffer-modified-p (not (equal s (bison--current-line))))))

;;;; Motion Commands

(defun bison-forward-production ()
  "Move forward to the start of the next production."
  (interactive)
  (if (search-forward-regexp (rx bol (* space) (+ word) (* space) ":")
                             nil t)
      (goto-char (match-end 0))
    (when (called-interactively-p nil)
      (message "No more productions"))))

(defun bison-previous-production ()
  "Move forward to the start of the next production."
  (interactive)
  (let ((pt (point)))
    (goto-char (line-beginning-position))
    (if (search-backward-regexp (rx bol (* space) (+ word) (* space) ":")
                                nil t)
        (goto-char (match-end 0))
      ;; Revert position.
      (goto-char pt)
      (when (called-interactively-p nil)
        (message "No more productions")))))

;;;; Code block utilities

(defun bison--c-block-start ()
  "The position of the brace beginning a C block on the current line.
Nil if not found."
  (save-excursion
    (goto-char (line-beginning-position))
    (when (search-forward-regexp (rx (not (any "'")) "{")
                                 (line-end-position) t)
      (1+ (match-beginning 0)))))

(defun bison--c-block-end ()
  "The position of the brace ending a C block on the current line.
Nil if not found."
  (save-excursion
    (-when-let (open (bison--c-block-start))
      (goto-char open)
      (forward-sexp)
      (1- (point)))))

(defun bison--c-block-extents ()
  "Find the extents of the C code block, if any, for the production at point.
Return a cons of (START . END), which are buffer positions."
  (-when-let* ((start (bison--c-block-start))
               (end (bison--c-block-end)))
    (cons start end)))

(cl-defun bison--single-line-c-block? ((start . end))
  "Non-nil if the given extents are on the same line.
START and END are buffer positions."
  (equal (line-number-at-pos start) (line-number-at-pos end)))

(defun bison--point-to-column (pos)
  "Convert buffer position POS to a column number."
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun bison--c-block-start-cols ()
  "Find the starting column for C blocks in each case for this production.
Return a list of column numbers."
  (save-excursion
    (let (acc)
      (while (bison--forward-production-case)
        (-when-let (extents (bison--c-block-extents))
          (cl-destructuring-bind (start . end) extents
            (when (bison--single-line-c-block? extents)
              (setq acc (cons start acc))))))
      (-map 'bison--point-to-column (nreverse acc)))))

(defun bison--c-block-end-cols ()
  "Find the ending column C for blocks in each case for this production.
Return a list of column numbers."
  (save-excursion
    (let (acc)
      (while (bison--forward-production-case)
        (-when-let (extents (bison--c-block-extents))
          (cl-destructuring-bind (start . end) extents
            (when (bison--single-line-c-block? extents)
              (setq acc (cons end acc))))))
      (-map 'bison--point-to-column (nreverse acc)))))

;;;; Buffer Formatting

(defun bison--forward-production-case ()
  "Move forward to the next case in the production at point."
  (cond
   ((bison--at-end-of-production?) nil)

   ((or (bison--at-production-header?)
        (bison--at-first-production-case?))
    (forward-line)
    (back-to-indentation)
    (point))

   (t
    ;; Skip past C blocks.
    (-when-let (extents (bison--c-block-extents))
      (cl-destructuring-bind (_start . end) extents
        (goto-char end)))

    (forward-line)
    (back-to-indentation)
    (point))))

(defun bison--align-c-block-delimiters ()
  "Align the start and end delimiters of C blocks in the current production."
  (-when-let* ((block-close
                ;; The call to `-max' will error if there are no C blocks in the
                ;; current production. Use this property abort if there are no
                ;; blocks to align.
                (ignore-errors (-max (bison--c-block-end-cols))))
               (block-open
                (-max (cons bison-minimum-c-block-column
                            (bison--c-block-start-cols)))))

    (save-excursion
      (while (bison--forward-production-case)
        (-when-let (extents (bison--c-block-extents))
          (when (bison--single-line-c-block? extents)
            (cl-destructuring-bind (start . _end) extents
              ;; Indent the opening brace. Note that this will invalidate the
              ;; end of the block in the `extents' var.
              (goto-char start)
              (indent-to block-open))
            ;; Indent the closing brace. The end delimiter pos needs to be
            ;; recalculated as noted above.
            (goto-char (bison--c-block-end))
            (indent-to block-close)))))))

(defun bison--pad-c-block-delimiters ()
  "Apply inner padding to the C block in the current production case.
See `bison-apply-inner-padding-to-c-blocks?'."
  (save-excursion
    (-when-let (extents (bison--c-block-extents))
      (when (bison--single-line-c-block? extents)
        (cl-destructuring-bind (start . _end) extents
          ;; Pad opening. Note that this will invalidate the end of the block
          ;; in the `extents' var.
          (goto-char start)
          (search-forward "{")
          (just-one-space))
        ;; Pad closing. The end delimiter pos needs to be recalculated as noted
        ;; above.
        (goto-char (bison--c-block-end))
        (just-one-space)))))

(defun bison--reindent-c-block ()
  "Reindent the C block, if any, for the current production case.
Doing so forces the C block minimum column to be used in later
formatting steps."
  (save-excursion
    (-when-let (start (bison--c-block-start))
      (goto-char start)
      (just-one-space)
      (indent-to bison-minimum-c-block-column))))

(defun bison--format-production ()
  "Format the production at point.
Assume we are at the start of the production."
  (save-excursion
    ;; Apply pre-formatting to each production case.
    (save-excursion
      (while (bison--forward-production-case)
        (bison--reindent-c-block)
        (when bison-apply-inner-padding-to-c-blocks?
          (bison--pad-c-block-delimiters))))

    (bison--align-c-block-delimiters)))

(defun bison-format-buffer ()
  "Format the whole buffer.
Re-indents the buffer and aligns C code blocks in productions."
  (interactive)
  (let ((s (buffer-string)))
    (save-excursion
      ;; Indent buffer.
      (goto-char (point-min))
      (while (not (eobp))
        (bison-indent-line)
        (forward-line))
      ;; Format productions.
      (goto-char (point-min))
      (while (bison-forward-production)
        (bison--format-production)))
    ;; Clear dirty state if buffer was not actually modified.
    (let ((modified? (not (equal s (bison--current-line)))))
      (set-buffer-modified-p modified?)
      ;; Tell user whether buffer was modified.
      (when (called-interactively-p nil)
        (if modified?
            (message "Buffer formatted")
          (message "No changes"))))))

;;;; Mode Definition

;;;###autoload
(defvar bison-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "M-N") 'bison-forward-production)
    (define-key km (kbd "M-P") 'bison-previous-production)
    (define-key km (kbd "TAB") 'bison-indent-line)
    (define-key km (kbd "M-q") 'bison-format-buffer)
    km)
  "Keymap for `bison-mode'.")

;;;###autoload
(define-derived-mode bison-mode c-mode "Bison"
  "Major mode for editing bison/yacc files.

\\{bison-mode-map}"
  ;; Disable disruptive C minor modes.
  (c-toggle-auto-hungry-state -1)

  ;; Configure indentation and comments.
  (setq-local indent-line-function 'bison-indent-line)
  (setq-local c-electric-flag nil)

  ;; Configure font-lock.
  (setq-local font-lock-keywords nil)
  (setq-local font-lock-defaults '((bison-font-lock-keywords)
                                   nil nil nil)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.y$" . bison-mode))

(provide 'bison-mode)

;;; bison-mode.el ends here
