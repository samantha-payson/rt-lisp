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

(in-package io
  (std:use-package std

    (defun write (f io-list)
      (cond
        ((tuple? io-list)
         (let ((len (intrinsic:len io-list)))
           (rlet rec ((i 0)
                      (count 0))
             (if (< i len)
                 (rec (+ i 1)
                      (+ count (write f (intrinsic:get io-list i))))
               count))))
        ((cons? io-list)
         (let ((a (write f (car io-list)))
               (b (write f (cdr io-list))))
           (if (and (int28? a)
                    (int28? b))
               (+ a b)
             .io:error)))
        ((char? io-list)
         (write-char f io-list))
        ((nil? io-list)
         0)
        (T .io:error)))

    (defmacro with-open-file (name+arg* . body)
      (let ((name (car name+arg*))
            (arg* (cdr name+arg*)))
        (with-gensyms (result)
          `(let ((~name (io:open @arg*)))
             (let ((~result (progn @body)))
               (io:close ~name)
               ~result)))))

    (export with-open-file write)))
