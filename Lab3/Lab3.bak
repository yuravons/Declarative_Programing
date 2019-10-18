(define (func_size arg)
        (cond
          ((eq? arg '()) 0)
          (#t (+ (func_size (cdr arg)) 1))))

(define (func_list arg1 arg2)
        (cons arg1 (cons arg2 '())))

(define (func_level arg)
        (cond
          ((eq? arg '()) '())
          ((eq? (func_size arg) 1) cdr arg)
          (#t (func_list (func_level (cdr arg))(car arg)))))

(define (func_empty? arg)
        (cond
         ((> (func_size arg) 0) 0)
         (#t 1)))

(define (func_pair? arg)
        (cond
         ((pair? arg) 0)
         (#t 1)))

(define (func_verify_list arg)
        (cond ((eq? arg '()) '())
          ((pair? (cdr arg)) (cons (car arg) (func_verify_list (cdr arg))))
          ((list? (cdr arg)) arg)
          (#t (cons arg '()))))

(define (atom? arg)
        (cond
          ((list? arg) '#f)
          ((pair? arg) '#f)
          (#t '#t)))

(define (type arg)
        (cond
          ((list? arg)
           (cond
             ((> (func_size arg) 0) 'NOT_EMPTY_LIST)
             ((eq? (func_size arg) 0) 'EMPTY_LIST)))
             ((pair? arg) (cond ((atom? (cdr arg)) 'PAIR)
             ((pair? (cdr arg)) 'NOT_CORRECT_LIST)
             (#t 'PAIR)))
             (#t 'ATOM)))

(define (level arg)
        (cond
         ((eq? (atom? arg) '#t) "Атом")
         ((= (func_empty? (func_verify_list arg)) 1) "Пустий список")
         ((eq? (TYPE arg) 'PAIR) "Пара")
         ((< (func_size (func_verify_list arg)) 2) "У спискку менше двох елементів")
         (#t (func_level (func_verify_list arg)))))

(level 'a)
(level '())
(level '(a))
(level '(a . b))
(level '(a b . c))
(level '(a b c))
(level '(a b c d f g h))
(level '(a b c . d))
(level '(a (b c) d (f g e)))
(level '(a (b c) d (f g e) a . b))
(level '(a (b c) d (f g e) k l m (a . b)))
(level '((a . b) (c d) f g))