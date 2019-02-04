(defun map-to-neg (n)
  (if (lt 0 n)
      (insert (map-to-neg (isub n 1))
	      n
	      (isub 0 n))
      {}))

(map-to-neg 4096)
