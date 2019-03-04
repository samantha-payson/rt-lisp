(in-package io
  (std:use-package std

    (defun write (f io-list)
      (cond
        ((tuple? io-list)
         (let ((len (intrinsic:len io-list)))
           (rlet rec ((i 0)
                      (count 0))
             (when (< i len)
               (rec (+ i 1)
                    (+ count (write f (intrinsic:get io-list i))))))))
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
