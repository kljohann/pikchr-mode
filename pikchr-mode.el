;;; pikchr-mode.el --- A major mode for the pikchr diagram markup language -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Johann Klähn

;; Author: Johann Klähn <johann@jklaehn.de>
;; URL: https://github.com/kljohann/pikchr-mode
;; Keywords: languages
;; Version: 0
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify
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

;; A major mode for the pikchr (https://pikchr.org/) diagram markup language.

;;; Code:

(require 'rx)
(require 'regexp-opt)

(defconst pikchr-mode-font-lock-keywords
  (rx-let ((ws* (* space))
           (symb (| word (syntax symbol))))
    `(
      ;; Ordinals
      (,(rx bow (+ digit) (| "th" "rd" "nd" "st") eow)
       . font-lock-builtin-face)
      ;; Hexadecimal integer constants
      (,(rx bow "0x" (+ digit) eow)
       . font-lock-constant-face)
      ;; Decimal integer or floating point constants
      (,(rx
         bow
         (? (any "+-"))
         (| (: (+ digit) (? "." (+ digit)))
            (: "." (+ digit)))
         (? (any "eE")
            (? any "+-")
            (+ digit))
         (? (| "%" "in" "cm" "mm" "pt" "px" "pc"))
         eow)
       . font-lock-constant-face)
      ;; Variable definitions
      (,(rx bol ws* (group (+ symb)) ws* (? (any "-+*/"))"=")
       (1 font-lock-variable-name-face))
      ;; Place labels
      (,(rx bol ws* (group (any "A-Z") (* (any alnum "_"))) ws* ":")
       (1 font-lock-function-name-face))
      ;; Objects
      (,(regexp-opt
         '("arc" "arrow" "box" "circle" "cylinder" "dot" "ellipse" "file" "line"
         "move" "oval" "spline" "text") 'symbols)
       . font-lock-type-face)
      ;; Keywords
      (,(regexp-opt
         '("above" "abs" "aligned" "and" "as" "assert" "at" "behind" "below"
         "between" "big" "bold" "bot" "bottom" "c" "ccw" "center" "chop" "close"
         "color" "cos" "cw" "dashed" "define" "diameter" "dist" "dotted" "down"
         "e" "east" "end" "even" "fill" "first" "fit" "from" "go" "heading"
         "height" "ht" "in" "int" "invis" "invisible" "italic" "last" "left"
         "ljust" "max" "min" "n" "ne" "north" "nw" "of" "previous" "print" "rad"
         "radius" "right" "rjust" "s" "same" "se" "sin" "small" "solid" "south"
         "sqrt" "start" "sw" "t" "the" "then" "thick" "thickness" "thin" "to"
         "top" "until" "up" "vertex" "w" "way" "west" "wid" "width" "with"
         "x" "y" "<-" "->" "<->") 'symbols)
       . font-lock-keyword-face))))

(defconst pikchr-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?# "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table for `pikchr-mode'.")

(defconst pikchr-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `pikchr-mode'.")

;;;###autoload
(define-derived-mode pikchr-mode prog-mode "pikchr mode"
  "Major mode for the pikchr diagram markup language."
  :syntax-table pikchr-mode-syntax-table
  (setq font-lock-defaults '(pikchr-mode-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pikchr\\'" . pikchr-mode))

(provide 'pikchr-mode)
;;; pikchr-mode.el ends here
