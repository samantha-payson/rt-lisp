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

(in-package std

  (defun escaped? (x)
    (and (cons? x)
         (eq (car x) (quote std:escape))))

  (defun semiquoted? (x)
    (and (cons? x)
         (eq (car x) (quote std:semiquote))))

  (defun spliced? (x)
    (and (cons? x)
         (eq (car x) (quote std:splice))))

  (defun build-semiquote (x depth)
    (cond
      ((escaped? x)
        (if (zero? depth)
            (cadr x)
          (list (quote list)
                (quote (quote std:escape))
                (build-semiquote (cadr x) (pred depth)))))
      ((semiquoted? x)
        (list (quote list)
              (quote (quote std:semiquote))
            (build-semiquote (cadr x) (succ depth))))
      ((cons? x)
        (if (spliced? (car x))
            (if (zero? depth)
                (list (quote std:append)
                      (cadar x)
                      (build-semiquote (cdr x) depth))
              (list (quote std:cons)
                    (list (quote std:splice)
                          (build-semiquote (cadar x) (pred depth)))
                    (build-semiquote (cdr x) depth)))
          (list (quote std:cons)
                (build-semiquote (car x) depth)
                (build-semiquote (cdr x) depth))))
      ((tuple? x)
        (vfold (tpl [])
            ((elem (maptuple-1 (lambda (elem)
                                 (build-semiquote elem depth))
                               x)))
          (intrinsic:push-last tpl elem)))
      ((self-eval? x)
        x)
      (T
        (list (quote quote) x))))

  (defmacro semiquote (x)
    (build-semiquote x 0)))