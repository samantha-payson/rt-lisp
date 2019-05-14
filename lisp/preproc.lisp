;;;; This file is part of RT Lisp.
;;;;
;;;; RT Lisp is free software: you can redistribute it and/or modify it under
;;;; the terms of the GNU Lesser General Public License as published by the Free
;;;; Software Foundation, either version 3 of the License, or (at your option)
;;;; any later version.
;;;;
;;;; RT Lisp is distributed in the hope that it will be useful, but WITHOUT ANY
;;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;;; FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
;;;; more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public License
;;;; along with RT Lisp.  If not, see <https://www.gnu.org/licenses/>.

(require "lisp/io.lisp" "lisp/parser.lisp")

(package preproc { .use ( std )
                   .export ( preproc ) }

  (defun format-int (n)
    (if (zero? n)
        "0"
      (list->tuple
        (rlet rec ((n   n)
                   (digit* nil))
          (cond
            ((zero? n)
              digit*)
            ((< n 0)
              (cons '-' (rec (- n) nil)))
            (T
              (rec (/ n 10)
                   (cons ("0123456789" (% n 10))
                         digit*))))))))

  ;; .foo 57
  ;;
  ;;
  ;; C Code: someFun( ID(".foo") )
  ;;
  ;; Preproc'd code: someFun( ((rtl_Word)((57 << 4) | RTL_SELECTOR)) )
  ;;
  (defun preproc-parser ()
    (use-package parser
      (repeat
        (or (let ((_ "ID(\".")
                  (name (* (not-char ("\"" 0))))
                  (_ "\")"))
              (let ((id (code (selector nil (list->tuple name)))))
                ["((rtl_Word)((" (format-int id) " << 4) | RTL_SELECTOR))"]))
            (any-char)))))

  (defun preproc (in-path out-path)
    (io:with-open-file (in in-path .read)
      (io:with-open-file (out out-path .write)
        (io:write out
          (->> in
               iter:from-file
               (parser:parse (preproc-parser))))))))



