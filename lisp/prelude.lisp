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
      ((map? x)
        (fold-map (lambda (out k v)
                    (intrinsic:insert out
                      (build-semiquote k depth)
                      (build-semiquote v depth)))
                  {}
                  x))
      ((self-eval? x)
        x)
      (T
        (list (quote quote) x))))

  (defmacro semiquote (x)
    (build-semiquote x 0))

  (defun expand-uses (use* body)
    (if use*
        `(use-package ~(car use*)
           ~(expand-uses (cdr use*) body))
      body))

  (defun mapc-1 (fn ls)
    (rlet rec ((a ls))
      (when a
        (fn (car a))
        (rec (cdr a))))
    ls)

  (defun mapc-2 (fn ls0 ls1)
    (rlet rec ((a ls0)
               (b ls1))
      (when (and a b)
        (fn (car a) (car b))
                 (rec (cdr a) (cdr b))))
    ls1) ; Return rightmost list for easy use with ->>.
         ; Is this a good idea?

  (defun mapc-3 (fn ls0 ls1 ls2)
    (rlet rec ((a ls0)
               (b ls1)
               (c ls2))
      (when (and a b c)
        (fn (car a) (car b) (car c))
        (rec (cdr a) (cdr b) (cdr c))))
    ls2)

  (defun mapc-4 (fn ls0 ls1 ls2 ls3)
    (rlet rec ((a ls0)
               (b ls1)
               (c ls2)
               (d ls3))
      (when (and a b c d)
        (fn (car a) (car b) (car c) (car d))
        (rec (cdr a) (cdr b) (cdr c) (cdr d))))
    ls3)

  (defun mapc-n (fn ls*)
    (rlet rec ((ls* ls*))
      (unless (any nil? ls*)
        (apply-list fn (mapcar car ls*))
        (rec (mapcar cdr ls*))))
    (car (last ls*)))

  (defun mapc (fn . ls*)
    (mapc-n fn ls*))

  (defmacro mapc (fn . ls*)
    (cond
      ((eq (length ls*) 1)
        `(mapc-1 ~fn @ls*))
      ((eq (length ls*) 2)
        `(mapc-2 ~fn @ls*))
      ((eq (length ls*) 3)
        `(mapc-3 ~fn @ls*))
      ((eq (length ls*) 4)
        `(mapc-4 ~fn @ls*))
      (T
        `(mapc-n ~fn (list @ls*)))))

  (defmacro vmapc (letarg* . body)
    `(mapc (lambda ~(mapcar car letarg*)
             @body)
           @(mapcar cadr letarg*)))

  (defmacro package (name arg# . body)
    (let ((export*  (intrinsic:lookup arg# .export))
          (use*     (intrinsic:lookup arg# .use)))
      `(progn
         (in-package ~name
           ~(expand-uses use* `(progn (export @export*)
                                      @body))))))

  (defvar *loaded* nil)

  (defun require-1 (path)
    (with (*loaded*)
      (unless (any (lambda (ld)
                     (intrinsic:iso ld path))
                   *loaded*)
        (load path)
        (intrinsic:dyn-set *loaded*
                            (cons path
                                  *loaded*)))))

  (defun require path*
    (vmapc ((path path*))
      (require-1 path))
    (with (*loaded*)
      *loaded*))

  (defmacro -> (obj . fn*)
    (vfold (obj obj) ((fn fn*))
      (if (cons? fn)
          `(~(car fn) ~obj @(cdr fn))
        `(~fn ~obj))))

  (defmacro ->> (obj . fn*)
    (vfold (obj obj) ((fn fn*))
      (if (cons? fn)
          `(@fn ~obj)
        `(~fn ~obj))))

  (defun filter (fn ls)
    (rlet rec ((rev nil)
               (ls  ls))
      (cond
        ((nil? ls)
          (reverse rev))
        ((fn (car ls))
          (rec (cons (car ls) rev)
               (cdr ls)))
        (T
          (rec rev (cdr ls))))))

  (defmacro vfilter (name+arg . body)
    `(filter (lambda (~(car name+arg))
               @body)
       ~(cadr name+arg)))

  (defun last (ls)
    (cond
      ((nil? ls)
        nil)
      ((nil? (cdr ls))
        ls)
      (T (last (cdr ls)))))

  (defun butlast (ls)
    (rlet rec ((rev nil)
               (ls  ls))
      (cond
        ((nil? ls)
          nil)
        ((nil? (cdr ls))
          (reverse rev))
        (T (rec (cons (car ls) rev)
                (cdr ls))))))

  (defmacro xfilter (name . body+arg)
    `(filter (lambda (~name)
               @(butlast body+arg))
             @(last body+arg)))

  (defmacro xmapcar (name . body+arg)
    `(mapcar (lambda (~name)
               @(butlast body+arg))
             @(last body+arg)))

  (export package require -> ->>
          last butlast
          filter vfilter xfilter
          mapc-1 mapc-2 mapc-3 mapc-4 mapc vmapc
          xmapcar))