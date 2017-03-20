


(define (interp)
  (display "-----> ")
  (let ((expr (read)))
    (let ((value (myeval expr global-environment)))
      (display value)
      (newline)
      (interp))))

(newline)
(interp)
