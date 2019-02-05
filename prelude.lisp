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

(intrinsic:in-package std
 
  (intrinsic:use-package intrinsic
    (defun build-list (args)
      (if args
	  (cons 'cons
		(cons (car args)
		      (cons (build-list (cdr args))
			    nil)))))

    (defmacro list args
      (build-list args))

    (defun append2 (a b)
      (if (nil? a)
	  b
	  (cons (car a)
		(append2 (cdr a) b))))

    (defmacro and arg*
      (if (cons? arg*)
	  (if (cdr arg*)
	      (list 'if
		    (car arg*)
		    (cons 'and (cdr arg*)))
	    (car arg*))
	  T))

    (defun build-semiquote-tail (x)
      (if (cons? x)
	  (if (and (cons? (car x))
		   (eq (car (car x)) 'std:splice))
	      (list 'append2
		    (car (cdr (car x)))
		    (build-semiquote-tail (cdr x)))
	    (list 'cons
		  (build-semiquote (car x))
		  (build-semiquote-tail (cdr x))))
	(list 'quote x)))

    (defun build-semiquote (x)
      (if (cons? x)
	  (if (eq (car x) 'std:escape)
	      (car (cdr x))
	    (list 'cons
		  (build-semiquote (car x))
		  (build-semiquote-tail (cdr x))))
	(list 'quote x))))

  (intrinsic:defmacro semiquote (x)
    (build-semiquote x))

  (intrinsic:defmacro defmacro (name arg* . body)
    `(intrinsic:defmacro ~name ~arg*
       @body))

  (defmacro defun (name arg* . body)
    `(intrinsic:defun ~name ~arg*
       @body))

  (defmacro if (test then . else)
    `(intrinsic:if ~test
		   ~then
		   @else))

  (defmacro progn body
    `(intrinsic:progn @body))

  (defmacro when (test . body)
    `(if ~test
	 (progn @body)))

  (defun not (x)
    (intrinsic:nil? x))

  (defmacro unless (test . body)
    `(when (not ~test)
       @body))

  (defun car (x)
    (intrinsic:car x))

  (defun cdr (x)
    (intrinsic:cdr x))

  (defun caar (x)
    (car (car x)))

  (defun cadr (x)
    (car (cdr x)))

  (defun cdar (x)
    (cdr (car x)))

  (defun cddr (x)
    (cdr (cdr x)))

  (defun length (ls)
    (if ls
	(iadd 1 (length (cdr ls)))
      0))

  (defmacro lambda (arg* . body)
    `(intrinsic:lambda ~arg*
       @body))

  (defun mapcar-1 (fn ls)
    (when ls
      (intrinsic:cons (fn (car ls))
		      (mapcar-1 fn (cdr ls)))))

  (defun fold-1 (fn init ls)
    (if ls
	(fold-1 (fn init (car ls)) (cdr ls))
      init))

  (defmacro let (letarg* . body)
    `((lambda ~(mapcar-1 car letarg*)
	@body)
      @(mapcar-1 cadr letarg*)))

  (defmacro cond arm*
    (when arm*
      `(if ~(caar arm*)
	   (progn @(cdar arm*))
	   (cond @(cdr arm*)))))


  (intrinsic:export let)
  (intrinsic:export +)

  (defmacro + arg*
    (cond
      ((intrinsic:nil? arg*) 0)
      ((intrinsic:nil? (cdr arg*)) (car arg*))
      (T `(intrinsic:iadd ~(car arg*) (+ @(cdr arg*)))))))

(use-package std
  (let ((x 1)
	(y 2)
	(z 3))
    (+ x y z)))

(std:let ((a 1) (b 2) (c 3))
  (std:+ a b c))
