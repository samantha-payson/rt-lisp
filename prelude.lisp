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

  (defmacro definline (name arg* . body)
    `(progn
       (defmacro ~name ~arg* @body)
       (defun ~name ~arg*
	 (~name @arg*))))

  (defmacro progn body
    `(intrinsic:progn @body))

  (defmacro when (test . body)
    `(if ~test
	 (progn @body)))

  (definline car (x) `(intrinsic:car ~x))
  (definline cdr (x) `(intrinsic:cdr ~x))

  (definline caar (x) `(car (car ~x)))
  (definline cadr (x) `(car (cdr ~x)))
  (definline cdar (x) `(cdr (car ~x)))
  (definline cddr (x) `(cdr (cdr ~x)))

  (definline caaar (x) `(car (caar ~x)))
  (definline caadr (x) `(car (cadr ~x)))
  (definline cadar (x) `(car (cdar ~x)))
  (definline caddr (x) `(car (cddr ~x)))
  (definline cdaar (x) `(cdr (caar ~x)))
  (definline cdadr (x) `(cdr (cadr ~x)))
  (definline cddar (x) `(cdr (cdar ~x)))
  (definline cdddr (x) `(cdr (cddr ~x)))

  (definline caaaar (x) `(car (caaar ~x)))
  (definline caaadr (x) `(car (caadr ~x)))
  (definline caadar (x) `(car (cadar ~x)))
  (definline caaddr (x) `(car (caddr ~x)))
  (definline cadaar (x) `(car (cdaar ~x)))
  (definline cadadr (x) `(car (cdadr ~x)))
  (definline caddar (x) `(car (cddar ~x)))
  (definline cadddr (x) `(car (cdddr ~x)))
  (definline cdaaar (x) `(cdr (caaar ~x)))
  (definline cdaadr (x) `(cdr (caadr ~x)))
  (definline cdadar (x) `(cdr (cadar ~x)))
  (definline cdaddr (x) `(cdr (caddr ~x)))
  (definline cddaar (x) `(cdr (cdaar ~x)))
  (definline cddadr (x) `(cdr (cdadr ~x)))
  (definline cdddar (x) `(cdr (cddar ~x)))
  (definline cddddr (x) `(cdr (cdddr ~x)))

  (definline cons (car cdr)
    `(intrinsic:cons ~car ~cdr))

  (defmacro labels arg*
    `(intrinsic:labels @arg*))

  (defun reverse (ls)
    (labels ((rec (fwd rev)
	       (if fwd
		   (rec (cdr fwd) (cons (car fwd) rev))
		 rev)))
      (rec ls nil)))

  (defun mapcar-1 (fn ls)
    (labels ((rec (fwd rev)
	       (if fwd
		   (rec (cdr fwd)
			(cons (fn (car fwd))
			      rev))
		 (reverse rev))))
      (rec ls nil)))

  (defmacro rlet (name letarg* . body)
    `(intrinsic:labels ((~name ~(mapcar-1 car letarg*)
			  @body))
       (~name @(mapcar-1 cadr letarg*))))


  (defun mapcar-2 (fn ls0 ls1)
    (rlet rec ((fwd0 ls0)
	       (fwd1 ls1)
	       (rev nil))
      (if (and fwd0 fwd1)
	  (rec (cdr fwd0)
	       (cdr fwd1)
	       (cons (fn (car fwd0) (car fwd1))
		     rev))
	(reverse rev))))

  (defun mapcar-3 (fn ls0 ls1 ls2)
    (rlet rec ((fwd0 ls0)
	       (fwd1 ls1)
	       (fwd2 ls2)
	       (rev nil))
      (if (and fwd0 fwd1 fwd2)
	  (rec (cdr fwd0)
	       (cdr fwd1)
	       (cdr fwd2)
	       (cons (fn (car fwd0) (car fwd1) (car fwd2))
		     rev))
	(reverse rev))))

  (defun any (fn ls)
    (when ls
      (if (fn (car ls))
	  T
	  (any fn (cdr ls)))))

  (defun mapcar-n (fn . ls*)
    (rlet rec ((fwd* ls*)
	       (rev  nil))
      (if (any nil? fwd*)
	  (reverse rev)
	(rec (mapcar-1 cdr fwd*)
	     (cons (intrinsic:apply-list fn (mapcar-1 car fwd*))
		   rev)))))

  (defmacro lambda (arg* . body)
    `(intrinsic:lambda ~arg*
       @body))

  (defun fold-1 (fn init ls)
    (if ls
	(fold-1 (fn init (car ls)) (cdr ls))
      init))

  (defmacro export sym*
    `(progn
       @(mapcar-1 (lambda (sym)
  		    `(intrinsic:export ~sym))
  		  sym*)))

  (export list and semiquote defmacro defun definline if progn when lambda export

	  mapcar-1 mapcar-2 mapcar-3 mapcar-n

	  car cdr

	  caar cadr cdar cddr

	  caaar caadr cadar caddr
	  cdaar cdadr cddar cdddr

	  caaaar caaadr caadar caaddr
	  cadaar cadadr caddar cadddr
	  cdaaar cdaadr cdadar cdaddr
	  cddaar cddadr cdddar cddddr

	  nil? not unless)

  (export nil? symbol? selector? int28? fix14? tuple? string? map? cons?
	  top?
	  not unless)

  (definline nil?      (x) `(intrinsic:nil?      ~x))
  (definline symbol?   (x) `(intrinsic:symbol?   ~x))
  (definline int28?    (x) `(intrinsic:int28?    ~x))
  (definline fix14?    (x) `(intrinsic:fix14?    ~x))
  (definline tuple?    (x) `(intrinsic:tuple?    ~x))
  (definline cons?     (x) `(intrinsic:cons?     ~x))

  (definline not (x)
    `(nil? ~x))

  (defmacro unless (test . body)
    `(when (not ~test)
       @body))

  (export fold-1)


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

  (export with-gensyms rlambda rlet length mapcar)

  (defmacro rlambda (name arg* . body)
    `(intrinsic:labels ((~name ~arg* @body))
       ~name))

  (defun length (ls)
    (rlet rec ((ls ls)
	       (n  0))
      (if ls
	  (rec (cdr ls) (+ n 1))
	n)))


  (defun mapcar (fn . ls*)
    (intrinsic:apply-list mapcar-n (cons fn ls*)))

  (defmacro mapcar (fn . ls*)
    (cond
      ((eq (length ls*) 1)
       `(mapcar-1 ~fn @ls*))
      ((eq (length ls*) 2)
       `(mapcar-2 ~fn @ls*))
      ((eq (length ls*) 3)
       `(mapcar-3 ~fn @ls*))
      (T
       `(mapcar-n ~fn @ls*))))

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

