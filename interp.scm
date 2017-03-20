(define (invalid string expr)
  (display string)
  (error expr)
  (newline))
;(define (eval-let expr env)

  ;)

(define (make-environ)
  (let ((contents '()))
    (lambda (method-name)
      (cond
        ((eq? method-name 'push)
          (lambda (x) (set! contents (cons x contents))))
        ((eq? method-name 'lkp)
          (lambda (x) (eval-symbol x contents)))
        ((eq? method-name 'show)
          (lambda () (contents)))
          (else (display "invalid method"))
          ))))

(define (eval-if expr env)
  (if (if? expr)
      (if (myeval (car (cdr expr)) env)
          (myeval (car (cdr (cdr expr))) env)
          (myeval (car (cdr (cdr (cdr expr)))) env))
      (invalid "This is an invalid if: " expr)
      ))

(define (if? expr)
  (= (length expr) 4))

(define (eval-symbol symbol env)
  ;symbol lookup
  (if (null? env)
      (invalid "Undefined symbol: " symbol)
      (if (eq? (car (car env)) symbol)
          (cdr (car env))
          (eval-symbol symbol (cdr env)))))

(define global-environment
  (make-environ))



(define (eval-define! expr env)
  (display "I'm defining ")
  (if (list? expr)
    (expr)
    ((cons (cons (car(cdr expr)) (car(cdr (cdr expr)))) env))
  ))

;  (if (list? (cdr expr))
;    )



;  (cdr expr)
;  )

(define (myeval expr env)
  (display "I'm going to eval: ")
  (display expr)
  (newline)
  (cond
;   (null? expr) think about this
   ((boolean? expr) expr)
   ((string? expr) expr)
   ((symbol? expr) ((env 'lkp) expr)
   ((list? expr)
    (cond
     ((symbol? (car expr))
      (cond
       ((eq? (car expr) 'quote) (car (cdr expr)))
       ((eq? (car expr) 'if) (eval-if expr env))
       ((eq? (car expr) 'lambda) (eval-lambda expr env))
       ((eq? (car expr) 'define) (eval-define! expr env))
       ((eq? (car expr) 'exit) (exit))
       ))))))



(define (interp)
  (display "-----> ")
  (let ((expr (read)))
    (let ((value (myeval expr global-environment)))
      (display value)
      (newline)
      (interp))))


(newline)
(interp)
