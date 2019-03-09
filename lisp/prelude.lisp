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

    (defun list args
      args)

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

    (defun and arg*
      (fold-1 (lambda (a b)
                (and a b))
              T
              arg*))

    (defun self-eval? (x)
      (if (nil? x)
          T
        (if (selector? x)
            T
          (if (int28? x)
              T
            (if (fix14? x)
                T
              nil)))))

    (defun build-bootstrap-semiquote-tail (x)
      (if (cons? x)
          (if (and (cons? (car x))
                   (eq (car (car x)) (quote std:splice)))
              (list (quote append2)
                    (car (cdr (car x)))
                    (build-bootstrap-semiquote-tail (cdr x)))
            (list (quote cons)
                  (build-bootstrap-semiquote (car x))
                  (build-bootstrap-semiquote-tail (cdr x))))
        (if (self-eval? x)
            x
          (list (quote quote) x))))

    (defun build-bootstrap-semiquote (x)
      (if (cons? x)
          (if (eq (car x) (quote std:escape))
              (car (cdr x))
            (list (quote cons)
                  (build-bootstrap-semiquote (car x))
                  (build-bootstrap-semiquote-tail (cdr x))))
        (if (self-eval? x)
            x
          (list (quote quote) x)))))

  (intrinsic:defmacro semiquote (x)
    (build-bootstrap-semiquote x))

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

  (defmacro quote (x)
    `(intrinsic:quote ~x))

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

  (defun mapcar-4 (fn ls0 ls1 ls2 ls3)
    (rlet rec ((fwd0 ls0)
               (fwd1 ls1)
               (fwd2 ls2)
               (fwd3 ls3)
               (rev nil))
      (if (and fwd0 fwd1 fwd2)
          (rec (cdr fwd0)
               (cdr fwd1)
               (cdr fwd2)
               (cdr fwd3)
               (cons (fn (car fwd0) (car fwd1) (car fwd2) (car fwd3))
                     rev))
        (reverse rev))))

  (defun any (fn ls)
    (when ls
      (if (fn (car ls))
          T
          (any fn (cdr ls)))))

  (defun all (fn ls)
    (if ls
        (when (fn (car ls))
          (all fn (cdr ls)))
        T))

  (defun mapcar-n (fn ls*)
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
        (fold-1 fn (fn init (car ls))
                (cdr ls))
      init))

  (defun fold-2 (fn init ls0 ls1)
    (if ls
        (fold-2 fn (fn init (car ls0) (car ls1))
                (cdr ls0)
                (cdr ls1))
      init))

  (defun fold-3 (fn init ls0 ls1 ls2)
    (if ls
        (fold-3 fn (fn init (car ls0) (car ls1) (car ls2))
                (cdr ls0)
                (cdr ls1)
                (cdr ls2))
      init))

  (defun fold-4 (fn init ls0 ls1 ls2 ls3)
    (if ls
        (fold-4 fn (fn init (car ls0) (car ls1) (car ls2) (car ls3))
                (cdr ls0)
                (cdr ls1)
                (cdr ls2)
                (cdr ls3))
      init))

  (defun fold-n (fn init ls*)
    (if (any nil? ls*)
        init
        (fold-n fn
                (intrinsic:apply-list fn (cons init (mapcar car ls*)))
                (mapcar cdr ls*))))

  (defmacro export sym*
    `(progn
       @(mapcar-1 (lambda (sym)
                    `(intrinsic:export ~sym))
                  sym*)))



  (definline nil?      (x) `(intrinsic:nil?      ~x))
  (definline symbol?   (x) `(intrinsic:symbol?   ~x))
  (definline selector? (x) `(intrinsic:selector? ~x))
  (definline int28?    (x) `(intrinsic:int28?    ~x))
  (definline fix14?    (x) `(intrinsic:fix14?    ~x))
  (definline tuple?    (x) `(intrinsic:tuple?    ~x))
  (definline char?     (x) `(intrinsic:char?     ~x))
  (definline map?      (x) `(intrinsic:map?      ~x))
  (definline cons?     (x) `(intrinsic:cons?     ~x))
  ;; (definline function? (x) `(intrinsic:function? ~x))
  ;; (definline closure?  (x) `(intrinsic:closure?  ~x))
  ;; (definline native?   (x) `(intrinsic:native?   ~x))
  (definline top?      (x) `(intrinsic:top?      ~x))

  (definline not (x)
    `(nil? ~x))

  (defmacro unless (test . body)
    `(when (not ~test)
       @body))

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

  (definline gensym ()
    `(intrinsic:gensym))

  (defmacro + arg*
    (cond
      ((nil? arg*) 0)
      ((nil? (cdr arg*))
        (car arg*))
      (T 
        `(+ (intrinsic:iadd ~(car arg*) ~(cadr arg*))
            @(cddr arg*)))))

  (defun + arg*
    (fold-1 (lambda (x y)
              (intrinsic:iadd x y))
            0
            arg*))

  (defmacro - arg*
    (cond
      ((nil? arg*) 0)
      ((nil? (cdr arg*))
        `(intrinsic:isub 0 ~(car arg*)))
      ((nil? (cddr arg*))
        `(intrinsic:isub ~(car arg*) ~(cadr arg*)))
      (T
        `(- (intrinsic:isub ~(car arg*) ~(cadr arg*))
            @(cddr arg*)))))
  
  (defun - (first . rest*)
    (if rest*
        (fold-1 (lambda (x y)
                  (intrinsic:isub x y))
                first
                rest)
      (intrinsic:isub 0 first)))

  (defmacro * arg*
    (cond
      ((nil? arg*) 1)
      ((nil? (cdr arg*))
        (car arg*))
      (T
        `(* (intrinsic:imul ~(car arg*) ~(cadr arg*))
            @(cddr arg*)))))

  (defun * arg*
    (fold-1 (lambda (x y)
              (* x y))
            1
            arg*))

  (defmacro / arg*
    (cond
      ((nil? arg*) 1)
      ((nil? (cdr arg*))
        `(intrinsic:idiv 1 ~(car arg*)))
      ((nil? (cddr arg*))
        `(intrinsic:idiv ~(car arg*) ~(cadr arg*)))
      (T
        `(/ (intrinsic:idiv ~(car arg*) ~(cadr arg*))
            @(cddr arg*)))))

  (defun / (first . rest*)
    (if rest*
        (fold-1 (lambda (x y)
                  (intrinsic:idiv x y))
                first
                rest*)
      (intrinsic:idiv 1 first)))

  (definline succ (x)
    `(intrinsic:iadd ~x 1))

  (definline pred (x)
    `(intrinsic:isub ~x 1))

  (defmacro with-gensyms (g* . body)
    `(let ~(mapcar-1 (lambda (g)
                       `(~g (gensym)))
                     g*)
       @body))

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

  (defmacro <= (first . rest*)
    (cond
      ((nil? rest*)
       T)
      ((nil? (cdr rest*))
       `(intrinsic:leq ~first ~(car rest*)))
      (T
       (with-gensyms (tmp)
         `(let ((~tmp ~(car rest*)))
            (if (<= ~first ~tmp)
                (<= ~tmp @(cdr rest*))))))))

  (defmacro >= (first . rest*)
    (cond
      ((nil? rest*)
       T)
      ((nil? (cdr rest*))
       `(intrinsic:geq ~first ~(car rest*)))
      (T
       (with-gensyms (tmp)
         `(let ((~tmp ~(car rest*)))
            (if (>= ~first ~tmp)
                (>= ~tmp @(cdr rest*))))))))

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

  (definline zero? (x)
    `(eq 0 ~x))

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
    (mapcar-n fn ls*))

  (defmacro mapcar (fn . ls*)
    (cond
      ((eq (length ls*) 1)
       `(mapcar-1 ~fn @ls*))
      ((eq (length ls*) 2)
       `(mapcar-2 ~fn @ls*))
      ((eq (length ls*) 3)
       `(mapcar-3 ~fn @ls*))
      ((eq (length ls*) 4)
       `(mapcar-4 ~fn @ls*))
      (T
       `(mapcar-n ~fn (list @ls*)))))

  (defmacro vmapcar (letarg* . body)
    `(mapcar (lambda ~(mapcar car letarg*)
               @body)
             @(mapcar cadr letarg*)))

  (defun fold (fn init . ls*)
    (intrinsic:apply-list fold-n (cons fn (cons init ls*))))

  (defmacro fold (fn init . ls*)
    (cond
      ((eq (length ls*) 1)
       `(fold-1 ~fn ~init @ls*))
      ((eq (length ls*) 2)
       `(fold-2 ~fn ~init @ls*))
      ((eq (length ls*) 3)
       `(fold-3 ~fn ~init @ls*))
      ((eq (length ls*) 4)
       `(fold-4 ~fn ~init @ls*))
      (T
       `(fold-n ~fn ~init @ls*))))

  (defmacro vfold (init letarg* . body)
    `(fold (lambda (~(car init) @(mapcar car letarg*))
             @body)
           ~(cadr init)
           @(mapcar cadr letarg*)))



  (defmacro in-package (name . body)
    `(intrinsic:in-package ~name
       @body))

  (defmacro use-package (name . body)
    `(intrinsic:use-package ~name
       @body))

  (defmacro alias-package (clause . body)
    `(intrinsic:alias-package ~clause
       @body))

  (defun maptuple-1 (fn tpl)
    (let ((len (intrinsic:len tpl)))
      (rlet rec ((i 0)
                 (rev nil))
        (if (< i len)
            (rec (+ i 1) (cons (fn (intrinsic:get tpl i)) rev))
          (reverse rev)))))

  (defun min (first . rest*)
    (rlet rec ((best  first)
               (rest* rest*))
      (if rest*
          (if (< (car rest*) best)
              (rec (car rest*) (cdr rest*))
            (rec best (cdr rest*)))
        best)))

  (defun mapt-1 (fn tpl)
    (let ((len (intrinsic:len tpl)))
      (rlet rec ((i 0))
        (when (< i len)
          (fn (intrinsic:get tpl i))
          (rec (+ i 1))))))

  (defun mapt-2 (fn tpl0 tpl1)
    (let ((len (min (intrinsic:len tpl0)
                    (intrinsic:len tpl1))))
      (rlet rec ((i 0))
        (when (< i len)
          (fn (intrinsic:get tpl0 i)
              (intrinsic:get tpl1 i))
          (rec (+ i 1))))))

  (defun mapt-3 (fn tpl0 tpl1 tpl2)
    (let ((len (min (intrinsic:len tpl0)
                    (intrinsic:len tpl1)
                    (intrinsic:len tpl2))))
      (rlet rec ((i 0))
        (when (< i len)
          (fn (intrinsic:get tpl0 i)
              (intrinsic:get tpl1 i)
              (intrinsic:get tpl2 i))
          (rec (+ i 1))))))

  (defun mapt-4 (fn tpl0 tpl1 tpl2 tpl3)
    (let ((len (min (intrinsic:len tpl0)
            (intrinsic:len tpl1)
            (intrinsic:len tpl2)
            (intrinsic:len tpl3))))
      (rlet rec ((i 0))
        (when (< i len)
          (fn (intrinsic:get tpl0 i)
              (intrinsic:get tpl1 i)
              (intrinsic:get tpl2 i)
              (intrinsic:get tpl3 i))
          (rec (+ i 1))))))

  (defun mapt-n (fn tpl*)
    (let ((len (apply-list min tpl*)))
      (rlet rec ((i 0))
        (when (< i len)
               (apply-list fn (vmapcar ((tpl tpl*))
                           (intrinsic:get tpl i)))
          (rec (+ i 1))))))

  (defun mapt (fn . tpl*)
    (mapt-n fn tpl*))

  (defmacro mapt (fn . tpl*)
    (cond
      ((eq (length tpl*) 1)
       `(mapt-1 ~fn @tpl*))
      ((eq (length tpl*) 2)
       `(mapt-2 ~fn @tpl*))
      ((eq (length tpl*) 3)
       `(mapt-3 ~fn @tpl*))
      ((eq (length tpl*) 4)
       `(mapt-4 ~fn @tpl*))
      (T
       `(mapt-n ~fn (list @tpl*)))))

  (defmacro vmapt (letarg* . body)
    `(mapt (lambda ~(mapcar car letarg*)
             @body)
           @(mapcar cadr letarg*)))

  (defmacro with (var* . body)
    `(let ~(mapcar (lambda (var)
                     `(~var (intrinsic:dyn-get ~var)))
                   var*)
       @body))

  (defmacro bind (letarg* . body)
    (if letarg*
        `(intrinsic:bind ~(car letarg*)
           (bind ~(cdr letarg*)
             @body))
      `(progn @body)))

  (defmacro defvar (name value)
    `(intrinsic:dyn-set ~name ~value))

  (defun append arg*
    (vfold (end nil) ((arg (reverse arg*)))
      (append2 arg end)))

  (export semiquote defmacro defun definline if progn when lambda export

    car cdr

    caar cadr cdar cddr

    caaar caadr cadar caddr
    cdaar cdadr cddar cdddr

    caaaar caaadr caadar caaddr
    cadaar cadadr caddar cadddr
      cdaaar cdaadr cdadar cdaddr
    cddaar cddadr cdddar cddddr

    append reverse list

    and not unless cons quote

    nil? symbol? selector? int28? fix14? tuple? char? map? cons? top?

    self-eval? atom?

    let cond gensym

    with-gensyms rlambda rlet length mapcar vmapcar fold vfold

    + - * / % succ pred

    < > <= >= eq zero?

    in-package use-package alias-package

    maptuple-1 mapt vmapt

    fold-map

    with bind defvar))