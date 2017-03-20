(define (invalid string expr)
  (display string)
  (error expr)
  (newline))

(define (if? expr)
  (= (length expr) 4))

(define (eval-symbol symbol env)
  (if (null? env)
      (invalid "Undefined symbol: " symbol)
      (if (eq? (car (car env)) symbol)
          (cdr (car env))
          (eval-symbol symbol (cdr env)))))


(define (eval-if expr env)
  (if (if? expr)
      (if (myeval (car (cdr expr)) env)
          (myeval (car (cdr (cdr expr))) env)
          (myeval (car (cdr (cdr (cdr expr)))) env))
      (invalid "This is an invalid if: " expr)
      ))

(define (lambda? expr)
  #t)

(define (eval-lambda expr env)
  (if (lambda? expr)
      (list '_________________closure expr env)
      (invalid "This is an invalid lambda: " expr)
      ))

(define global-environment
  (list
   (cons 'Rick 10)
   ))

(define (myeval expr env)
  (display "I'm going to eval: ")
  (display expr)
  (newline)
  (cond
;   ((null? expr) think about this
   ((boolean? expr) expr)
   ((string? expr) expr)
   ((symbol? expr) (eval-symbol expr env))
   ((list? expr)
    (cond
     ((symbol? (car expr))
      "My car was a symbol"
      (cond
       ((eq? (car expr) 'quote) (car (cdr expr)))
       ((eq? (car expr) 'if) (eval-if expr env))
       ((eq? (car expr) 'lambda) (eval-lambda expr env))
       )
;      (display expr)
      )
     )
    )
   (else "I am undefined so far"
         (display expr))
   ))

(define (interp)
  (display "-----> ")
  (let ((expr (read)))
    (let ((value (myeval expr global-environment)))
      (display "Result: ")
      (display value)
      (newline)
      (interp))))

(newline)
(interp)
