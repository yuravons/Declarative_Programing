(DEFINE (FUNC_EMPTY? arg)
        (COND
          ((> (Func_Size arg) 0) 0)
          (#t 1)))

(DEFINE (Func_Size arg)
        (COND
          ((EQ? arg '()) 0)
          (#t (+ (Func_Size (CDR arg)) 1))))

(DEFINE (Func_Contain_Atom? arg)
        (COND
          ((LIST? arg) 0)
          ((PAIR? arg) 0)
          (#t 1)))

(DEFINE (Func_Contain_Pair arg)
        (COND
          ((= (Func_Size arg) 1)
          (PAIR? arg))
          (#t #f)))

(DEFINE (Func_Search_Last arg)
        (COND
          ((EQ? arg '()) '())
          ((EQ? (CDR arg) '()) (CAR arg))
          (#t (Func_Search_Last (CDR arg)))))

(DEFINE (Func_Combine arg)
        (COND
          ((EQ? arg '()) '())
          ((LIST? (CDR arg)) arg)
          ((PAIR? (CDR arg))
           (CONS (CAR arg)(Func_Combine (CDR arg))))
          (#t (CONS arg '()))))

(DEFINE (LAST X)
        (COND
         ((> (Func_Contain_Atom? X) 0) "Один атом")
         ((= (FUNC_EMPTY? (Func_Combine X)) 1) "Пустий список")
         ((Func_Contain_Pair (Func_Combine X)) "Одна пара")
         (#t (Func_Search_Last (Func_Combine X)))))