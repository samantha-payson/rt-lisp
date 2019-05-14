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

(require "lisp/iter.lisp")

(package parser
    { .export ( parse char-pred char any-char not-char cons list const or
                apply call let not without-prefix
                repeat * + ? ) }
  
  (std:defmacro with-parsers (p* . body)
    `(std:let ~(std:vmapcar ((p p*))
                 `(~p (ensure-parser ~p)))
       @body))

  (std:defun run (p itr.p)
    (p itr.p
       (std:lambda (x p.itr)
         x)
       (std:lambda ()
         .parser:error)))

  (std:defmacro parse (p itr.p)
    `(run (std:use-package parser (ensure-parser ~p))
          ~itr.p))

  (std:defun char-pred (fn)
    (std:use-package std
      (lambda (itr.c pass fail)
        (if (nil? itr.c)
            (fail)
          (let ((c (iter:car itr.c)))
            (if (fn c)
              (pass c (iter:cdr itr.c))
              (fail)))))))

  (std:defun any-char ()
    (char-pred (std:lambda (_) T)))

  (std:defun char (c)
    (char-pred (std:lambda (x) (std:eq c x))))

  (std:defun not-char c*
    (char-pred (std:lambda (x)
                 (std:not (std:any (std:lambda (c)
                                     (std:eq c x))
                                   c*)))))

  (std:defun eof ()
    (std:lambda (itr pass fail)
      (std:use-package std
        (if (nil? itr)
            (pass nil itr)
          (fail)))))

  (std:defun const (x)
    (std:lambda (itr pass fail)
      (pass x itr)))

  (std:defun always-fail ()
    (std:lambda (itr pass fail)
      (fail)))

  (std:defun cons (a b)
    (with-parsers (a b)
      (std:lambda (itr.a.b pass fail)
        (a itr.a.b
           (std:lambda (a a.itr.b)
             (b a.itr.b
                (std:lambda (b a.b.itr)
                  (pass (std:cons a b)
                        a.b.itr))
                fail))
           fail))))

  (std:defun list p*
    (std:vfold (acc (const nil))
        ((p (std:reverse p*)))
      (cons p acc)))

  (std:defun or2 (a b)
    (with-parsers (a b)
      (std:lambda (itr.a|b pass fail)
        (a itr.a|b
           pass
           (std:lambda ()
             (b itr.a|b
                pass
                fail))))))

  (std:defun or p*
    (std:vfold (acc (always-fail)) ((p (std:reverse p*)))
      (or2 p acc)))

  (std:defun apply (fn p)
    (with-parsers (p)
      (std:lambda (itr.p pass fail)
        (p itr.p
           (std:lambda (arg* p.itr)
             (pass (intrinsic:apply-list fn arg*)
                   p.itr))
           fail))))

  (std:defmacro call (fn . arg*)
    `(apply ~fn (list @arg*)))

  (std:defun not (p)
    (with-parsers (p)
      (std:lambda (itr.~p pass fail)
        (p itr.~p
           (std:lambda (x p.itr)
             (fail))
           (std:lambda ()
             (pass nil itr.~p))))))

  (std:defmacro let (letarg* . body)
    `(call (std:lambda ~(std:mapcar std:car letarg*)
             (std:use-package std
               @body))
           @(std:mapcar std:cadr letarg*)))

  (std:defun without-prefix (pre p)
    (let ((_ (not pre))
          (x p))
      x))

  (std:defun repeat (p .key (lo 0) hi)
    (std:use-package std
      (with-parsers (p)
        (lambda (itr.p* pass fail)
          (p itr.p*
             (rlet rec ((n   1)
                        (rev nil))
               (cond
                 ((and hi (eq hi n))
                   (lambda (val p*.itr)
                     (pass (reverse (cons val rev))
                           p*.itr)))
                 (T
                   (lambda (val p*.itr.p*)
                     (p p*.itr.p*
                        (rec (succ n)
                             (cons val rev))
                        (if (< n lo)
                            fail
                          (lambda ()
                            (pass (reverse (cons val rev))
                                  p*.itr.p*))))))))
             (if (< 0 lo)
                 fail
               (lambda ()
                 (pass nil itr.p*))))))))

  (std:defun * (p)
    (repeat p))

  (std:defun + (p)
    (repeat p .lo 1))

  (std:defun ? (p)
    (repeat p .hi 1))

  (std:defun ensure-parser (p)
    (std:cond
      ((std:char? p)
        (char p))
      ((std:tuple? p)
        (call std:list->tuple (intrinsic:apply-tuple list p)))
      ((std:cons? p)
        (cons (std:car p) (std:cdr p)))
      ((std:nil? p)
        (const nil))
      (T p))))
