;;; cnc-mode.el --- CNC mode -*- lexical-binding: t -*-
;; TODO: test with nc files from other machine vendors
;; TODO: set up travis (or similar)
;; TODO: test on older Emacsen

;; Copyright © 2019 Matthijs Kool

;; Author: Matthijs Kool <matthijzk@gmail.com>
;; Created: 17 Feb 2019
;; Version: 0.1
;; Keywords: languages
;; URL: https://github.com/matthijsk/cnc-mode

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Major mode for editing CNC files.

;;; Code:

(defgroup cnc nil "CNC mode."
  :prefix "cnc-")

(defcustom cnc-line-number-start 10
  "Number at which automatic line numbering of CNC files will start."
  :type 'integer
  :group 'cnc)

(defcustom cnc-line-number-increment 10
  "Automatic CNC line numbering increment value."
  :type 'integer
  :group 'cnc)

(defcustom cnc-line-number-padding t
  "Left padding for CNC line numbers.
When t, line numbers will be padded at the left with zeroes. The
padding width is dependent on the total number of lines."
  :type 'boolean
  :group 'cnc)

(defcustom cnc-line-number-append-string "   "
  "String appended after an automatically inserted CNC line number.
Anything other than whitespace would not make sense."
  :type 'string
  :group 'cnc)

(defun cnc-number-of-digits (number)
  "Return the number of digits in the integer NUMBER."
  (if (integerp number)
      (let ((number (abs
                     (if (= number most-negative-fixnum)
                         (1+ number)
                       number)))
            (count 0))
        (while (> number 0)
          (setq number (/ number 10))
          (setq count (1+ count)))
        count)
    (signal 'wrong-type-argument '(integerp number))))

(defun cnc-remove-line-numbers ()
  "Remove line numbers from a CNC buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let* ((progress-reporter
              (make-progress-reporter
               "Removing line numbers..." (point-min) (point-max))))
        ;; Match linenumbers starting with N and a positive or negative number.
        ;; For example N320 or N-1240.
        (while (re-search-forward "^[[:blank:]]*N-?[[:digit:]]+[[:blank:]]+" nil t)
          (replace-match "")
          (progress-reporter-update progress-reporter (point)))
        (progress-reporter-done progress-reporter)))))

(defun cnc-renumber-lines ()
  "Automatically renumber lines in a CNC buffer.
The first line in the buffer will be prefixed with an 'N', padded
with optional 0's if `cnc-line-number-padding' is t, the value
set in `cnc-line-number-start' and finally the string
`cnc-line-number-append-string'. The number for each subsequent
line will be incremented from the previous value with
`cnc-line-number-increment'."
  (interactive)
  (cnc-remove-line-numbers)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let* ((cnc-current-line-number cnc-line-number-start)
             (last-line (line-number-at-pos (point-max)))
             (format-string
              ;; Set format string to N%0<width>d%s if padding is enabled.
              ;; Otherwise, set it to N%s.
              ;; (concat "N%0"
              (concat "N%"
                      (when cnc-line-number-padding
                        (concat "0"
                                (number-to-string
                                 (cnc-number-of-digits
                                  (* last-line cnc-line-number-increment)))))
                      "d%s")))
        (dotimes-with-progress-reporter (i last-line)
            "Renumbering lines..."
          (insert
           (format format-string
                   cnc-current-line-number cnc-line-number-append-string))
          (setq cnc-current-line-number
                (+ cnc-current-line-number cnc-line-number-increment))
          (forward-line))))))

(defun cnc-next-statement (&optional arg)
  "Move point forward to next cnc statement.
With prefix arg, go forward N + 1 statements. Move forward to the
end of the next statement if already at end."
  (interactive "p")
  (re-search-forward "[GMT][[:digit:]]+" nil t arg))

(defun cnc-previous-statement (&optional arg)
  "Move point backward to start of current cnc statement.
With prefix arg, go back N - 1 statements. If already at the
beginning of a statement go to the beginning of the closest
preceding one."
  (interactive "p")
  (cnc-next-statement (- arg)))

(defvar cnc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") 'cnc-remove-line-numbers)
    (define-key map (kbd "C-c C-r") 'cnc-renumber-lines)
    (define-key map (kbd "M-e")     'cnc-next-statement)
    (define-key map (kbd "M-a")     'cnc-previous-statement)
    map)
  "Keymap for CNC mode.")

(setq cnc-mode-highlights
      '(("[^[:alpha:]]\\((.*)\\)" . (1 font-lock-comment-face))
        (";.*$" . font-lock-comment-face)
        ("%.*$" . font-lock-comment-face)
        ("G[0-9]+" . font-lock-keyword-face)
        ("M[0-9]+" . font-lock-builtin-face)
        ("\\(T\\)-?[0-9]+" . (1 font-lock-builtin-face))
        ("\\([FS]\\)-?[0-9]+" . (1 font-lock-variable-name-face))
        ("\\([XYZPQRB]\\)-?[0-9]+" . (1 font-lock-variable-name-face))
        ("\\([IJK]\\)-?[0-9]+" . (1 font-lock-variable-name-face))
        ("\\(SIN\\|COS\\|TAN\\|ASIN\\|ACOS\\|ATAN\\|HSIN\\|HCOS\\|HTAN\\)(.*)" . (1 font-lock-function-name-face))
        ("\\(SQRT\\|ABS\\|EXP\\|LN\\|LOG\\|POW\\|HYPOT\\)(.*)" . (1 font-lock-function-name-face))
        ("\\(ROUND\\|FIX\\|FUP\\)(.*)" . (1 font-lock-function-name-face))
        ("ERROR\\|MOD" . font-lock-function-name-face)
        ("PI" . font-lock-constant-face)
        ("EQ\\|NE\\|GT\\|LT\\|GE\\|LE\\|AND\\|OR\\|XOR" . font-lock-builtin-face)
        ("\\(DEGREES\\|RADIANS\\)(.*)" . (1 font-lock-function-name-face))
        ("GOTO\\|IF\\|THEN\\|WHILE\\|DO\\|END" . font-lock-builtin-face)
        ("#[0-9]+" . font-lock-variable-name-face)))

(defcustom cnc-mode-hook nil
  "Hook run when entering CNC mode."
  :type 'hook
  :group 'cnc)

;;;###autoload
(define-derived-mode cnc-mode prog-mode "CNC"
  "Major mode for editing Style CNC G-codes.

\\{cnc-mode-map}"
  :group 'cnc
  (setq font-lock-defaults '(cnc-mode-highlights))
  (setq-local comment-start "; ")
  (setq-local comment-end   "")
  (setq-local block-comment-start "(")
  (setq-local block-comment-end ")")
  (setq-local require-final-newline 'visit-save))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cnc\\'" . cnc-mode))

(provide 'cnc-mode)

;;; cnc-mode.el ends here
