(define (func_atom? arg)
        (cond
         ((list? arg) '#f)
         ((pair? arg) '#f)
         (#t '#t)))

(define (func_pair? arg)
        (cond
         ((eq? arg '()) #f)
         ((func_atom? arg) #f)
         ((&& (!! (pair? (cdr arg))) (!! (list? (cdr arg)))) #t)
         (#t #f)))

(define (func_list? a)
        (cond
         ((eq? a '()) #t)
         ((&& (!! (func_pair? a)) (!! (func_atom? a))) #t)
         (#t #f)))


(define (&& a b)
        (cond
          ((eq? a #t)
           (cond
            ((eq? b #t) #t)
            (#t #f)))
          ('(#t) #f)))

(define (!! a)
        (cond
         ((eq? a #t) #f)
         (#t #t)))

(define (|| a b)
        (cond
         ((eq? a #t) #t)
         (#t b)))

(define (reduce x g a)
        (cond
         ((eq? x '()) a)
         ((func_atom? x) (g a x))
         ((|| (func_list? (car x)) (func_pair? (car x))) (g (reduce (car x) g a) (reduce (cdr x) g a)))
         (#t (g (car x) (reduce (cdr x) g a)))))

(define (func_sum arg1 arg2)
        (cond
         ((!! (number? arg1)) arg2)
         (#t (+ arg1 arg2))))

(define (func_multiply arg1 arg2)
        (cond
         ((!! (number? arg1)) arg2)
         (#t (* arg1 arg2))))

(define (func_sum_multiply arg)
        (cons(reduce arg func_sum 1)
             (cons(reduce arg func_multiply 1) '())))