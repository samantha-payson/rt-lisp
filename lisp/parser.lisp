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

(package parser
    { .use     ( std )
      .require ( "lisp/iter.lisp" ) ; .require not yet implemented ...
      .export  ( parse p-char p-seq p-const)}
  
  (defun ensure-parser (p)
    (cond
      ((char? p)
        (p-char p))
      (T p)))

  (defmacro with-parsers (p* . body)
    `(let ~(vmapcar ((p p*))
             `(~p (ensure-parser ~p)))
       @body))

  (defun parse (p itr|p)
    (p itr|p
       (lambda (x p|itr)
         x)
       (lambda ()
         .parse:error)))

  (defun p-char (c)
    (lambda (itr|c pass fail)
      (if (eq c (iter:car itr|c))
          (pass c (iter:cdr itr|c))
        (fail))))

  (defun p-const (x)
    (lambda (itr pass fail)
      (pass x itr)))

  (defun p-seq2 (a b)
    (with-parsers (a b)
      (lambda (itr|a|b pass fail)
        (a itr|a|b
           (lambda (a a|itr|b)
             (b a|itr|b
                (lambda (b a|b|itr)
                  (pass (cons a b)
                        a|b|itr))
                fail))
           fail))))

  (defun p-seq p*
    (vfold (acc (p-const nil))
        ((p (reverse p*)))
      (p-seq2 p acc))))