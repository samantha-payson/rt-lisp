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

(package iter
    { .export ( mcons cons cons?

                car cdr

                caar cadr cdar cddr

                caaar caadr cadar caddr
                cdaar cdadr cddar cdddr

                caaaar caaadr caadar caaddr
                cadaar cadadr caddar cadddr
                cdaaar cdaadr cdadar cdaddr
                cddaar cddadr cdddar cddddr

                from-list into-list
                from-file
                from-dir

                take drop take-while drop-while ) }

  (std:defun cons? (x)
    (std:and (std:tuple? x)
             (std:eq (intrinsic:get x 0)
                     .iter:cons)))

  (std:definline cons (a b)
    `(intrinsic:tuple .iter:cons
                      (std:lambda () ~a)
                      (std:lambda () ~b)))

  (std:definline car (x) `((intrinsic:get ~x 1)))

  (std:definline cdr (x) `((intrinsic:get ~x 2)))

  (std:definline caar (x) `(car (car ~x)))
  (std:definline cadr (x) `(car (cdr ~x)))
  (std:definline cdar (x) `(cdr (car ~x)))
  (std:definline cddr (x) `(cdr (cdr ~x)))

  (std:definline caaar (x) `(caar (car ~x)))
  (std:definline caadr (x) `(caar (cdr ~x)))
  (std:definline cadar (x) `(cadr (car ~x)))
  (std:definline caddr (x) `(cadr (cdr ~x)))
  (std:definline cdaar (x) `(cdar (car ~x)))
  (std:definline cdadr (x) `(cdar (cdr ~x)))
  (std:definline cddar (x) `(cddr (car ~x)))
  (std:definline cdddr (x) `(cddr (cdr ~x)))

  (std:definline caaaar (x) `(caaar (car ~x)))
  (std:definline caaadr (x) `(caaar (cdr ~x)))
  (std:definline caadar (x) `(caadr (car ~x)))
  (std:definline caaddr (x) `(caadr (cdr ~x)))
  (std:definline cadaar (x) `(cadar (car ~x)))
  (std:definline cadadr (x) `(cadar (cdr ~x)))
  (std:definline caddar (x) `(caddr (car ~x)))
  (std:definline cadddr (x) `(caddr (cdr ~x)))
  (std:definline cdaaar (x) `(cdaar (car ~x)))
  (std:definline cdaadr (x) `(cdaar (cdr ~x)))
  (std:definline cdadar (x) `(cdadr (car ~x)))
  (std:definline cdaddr (x) `(cdadr (cdr ~x)))
  (std:definline cddaar (x) `(cddar (car ~x)))
  (std:definline cddadr (x) `(cddar (cdr ~x)))
  (std:definline cdddar (x) `(cdddr (car ~x)))
  (std:definline cddddr (x) `(cdddr (cdr ~x)))

  (std:defun from-list (ls)
    (std:when ls
      (cons (std:car ls)
            (from-list (std:cdr ls)))))

  (std:defun into-list (itr)
    (std:rlet rec ((itr itr)
                   (rev nil))
      (std:if itr
          (rec (cdr itr)
               (std:cons (car itr) rev))
        (std:reverse rev))))

  (std:defmacro thunk (expr)
    (std:use-package std
      (with-gensyms (forced? value)
        `(let ((~forced? nil)
               (~value   nil))
           (lambda ()
             (if ~forced?
                 ~value
               (progn
                 (intrinsic:set-var ~forced? T)
                 (intrinsic:set-var ~value  ~expr))))))))

  (std:defmacro mcons (a b)
    `(intrinsic:tuple .iter:cons
                      (thunk ~a)
                      (thunk ~b)))

  (std:defun from-file (f)
    (std:use-package std
      (let ((c (io:read-char f)))
        (if (eq c .io:EOF)
            nil
          (mcons c (from-file f))))))

  (std:defun from-dir (stream)
    (std:use-package std
      (let ((d (io:read-dir stream)))
        (if (nil? d)
            nil
          (mcons d (from-dir stream))))))

  (std:defun take (n itr)
    (std:when (std:< 0 n)
      (cons (car itr)
            (take (std:- n 1) (cdr itr)))))

  (std:defun drop (n itr)
    (std:if (std:< 0 n)
        (drop (std:- n 1) (cdr itr))
      itr))

  (std:defun take-while (fn itr)
    (std:when (fn (car itr))
      (cons (car itr)
            (take-while fn (cdr itr)))))

  (std:defun drop-while (fn itr)
    (std:if (fn (car itr))
        (drop-while fn (cdr itr))
      itr)))
