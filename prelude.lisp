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
	  (cons (quote cons)
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
	      (list (quote if)
		    (car arg*)
		    (cons (quote and) (cdr arg*)))
	    (car arg*))
	  T))

    (defun build-semiquote-tail (x)
      (if (cons? x)
	  (if (and (cons? (car x))
		   (eq (car (car x)) (quote std:splice)))
	      (list (quote append2)
		    (car (cdr (car x)))
		    (build-semiquote-tail (cdr x)))
	    (list (quote cons)
		  (build-semiquote (car x))
		  (build-semiquote-tail (cdr x))))
	(list (quote quote) x)))

    (defun build-semiquote (x)
      (if (cons? x)
	  (if (eq (car x) (quote std:escape))
	      (car (cdr x))
	    (list (quote cons)
		  (build-semiquote (car x))
		  (build-semiquote-tail (cdr x))))
	(list (quote quote) x))))

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

  (defun car (x)
    (intrinsic:car x))

  (defun cdr (x)
    (intrinsic:cdr x))

  (defun mapcar-1 (fn ls)
    (when ls
      (intrinsic:cons (fn (car ls))
		      (mapcar-1 fn (cdr ls)))))

  (defmacro lambda (arg* . body)
    `(intrinsic:lambda ~arg*
       @body))

  (defmacro export sym*
    `(progn
       @(mapcar-1 (lambda (sym)
  		    `(intrinsic:export ~sym))
  		  sym*)))

  (export list and semiquote defmacro defun if progn when car cdr mapcar-1 lambda export)

  (export nil? not unless)

  (defun not (x)
    (nil? x))

  (defun nil? (x)
    (intrinsic:nil? x))

  (defmacro unless (test . body)
    `(when (not ~test)
       @body))

  (export caar cadr cdar cddr)
  
  (defun caar (x)
    (car (car x)))

  (defun cadr (x)
    (car (cdr x)))

  (defun cdar (x)
    (cdr (car x)))

  (defun cddr (x)
    (cdr (cdr x)))



  (export fold-1)

  (defun  fold-1 (fn init ls)
    (if ls
	(fold-1 (fn init (car ls)) (cdr ls))
      init))


  (export let cond gensym)

  (defmacro let (letarg* . body)
    `((lambda ~(mapcar-1 car letarg*)
	@body)
      @(mapcar-1 cadr letarg*)))

  (defmacro cond arm*
    (when arm*
      (if (intrinsic:eq (caar arm*) T)
	  `(progn @(cdar arm*))
	  `(if ~(caar arm*)
	       (progn @(cdar arm*))
	       (cond @(cdr arm*))))))

  (defun gensym ()
    (intrinsic:gensym))


  (export +)

  (defmacro + arg*
    (cond
      ((nil? arg*) 0)
      ((nil? (cdr arg*)) (car arg*))
      (T `(intrinsic:iadd ~(car arg*) (+ @(cdr arg*))))))


  (defmacro with-gensyms (g* . body)
    `(let ~(mapcar-1 (lambda (g)
		       `(~g (gensym)))
		     g*)
       @body))

  (export < > eq)

  (defmacro < (first . rest*)
    (cond
      ((nil? rest*)
       T)
      ((nil? (cdr rest*))
       `(intrinsic:lt ~first ~(car rest*)))
      (T
       (with-gensyms (tmp)
	 `(let ((~tmp ~(car rest*)))
	    (if (< ~first ~tmp)
		(< ~tmp @(cdr rest*))))))))

  (defmacro > (first . rest*)
    (cond
      ((nil? rest*)
       T)
      ((nil? (cdr rest*))
       `(intrinsic:gt ~first ~(car rest*)))
      (T
       (with-gensyms (tmp)
	 `(let ((~tmp ~(car rest*)))
	    (if (> ~first ~tmp)
		(> ~tmp @(cdr rest*))))))))

  (defmacro eq (first . rest*)
    (cond
      ((nil? rest*)
       T)
      ((nil? (cdr rest*))
       `(intrinsic:eq ~first ~(car rest*)))
      (T
       (with-gensyms (tmp)
	 `(let ((~tmp ~(car rest*)))
	    (if (eq ~first ~tmp)
		(eq ~tmp @(cdr rest*))))))))

  (export with-gensyms rlambda rlet length)

  (defmacro rlambda (name arg* . body)
    `(intrinsic:labels ((~name ~arg* @body))
       ~name))

  (defmacro rlet (name letarg* . body)
    `(intrinsic:labels ((~name ~(mapcar-1 car letarg*)
			  @body))
       (~name @(mapcar-1 cadr letarg*))))

  (defun length (ls)
    (rlet rec ((ls ls)
	       (n  0))
      (if ls
	  (rec (cdr ls) (+ n 1))
	n)))


  (export in-package use-package alias-package)

  (defmacro in-package (name . body)
    `(intrinsic:in-package ~name
       @body))

  (defmacro use-package (name . body)
    `(intrinsic:in-package ~name
       @body))

  (defmacro alias-package (clause . body)
    `(intrinsic:alias-package ~clause
       @body)))

