(DEFINE (FUNC_LIST arg1 arg2)
        (CONS arg1 (CONS arg2 '())))

(DEFINE (Func_Size arg)
        (COND
         ((EQ? arg '()) 0)
         (#t (+ (Func_Size (CDR arg)) 1))))

(DEFINE (FUNC_EMPTY? arg)
        (COND
         ((> (Func_Size arg) 0) 0)
         (#t 1)))

(DEFINE (Func_Atom? arg)
        (COND
         ((LIST? arg) 0)
         ((PAIR? arg) 0)
         (#t 1)))

(DEFINE (Func_VerifyList arg)
        (COND ((EQ? arg '()) '())
          ((PAIR? (CDR arg)) (CONS (CAR arg) (Func_VerifyList (CDR arg))))
          ((LIST? (CDR arg)) arg)
          (#t (CONS arg '()))))

(DEFINE (Func_Combine arg)
        (FUNC_LIST
         (FUNC_LIST (CAR arg)(CAR(CDR arg)))
          (CDR(CDR arg))))

(DEFINE (Func_Contain_Pair arg)
        (COND
          ((= (Func_Size arg) 1)
          (PAIR? arg))
          (#t #f)))

(DEFINE(QUADRAT arg)
       (COND
        ((> (Func_Atom? arg) 0) "Атом")
        ((Func_Contain_Pair (Func_VerifyList arg)) "Одна пара")
        ((= (FUNC_EMPTY? (Func_VerifyList arg)) 1) "Пустий список")
        (#t (Func_Combine (Func_VerifyList arg)))))