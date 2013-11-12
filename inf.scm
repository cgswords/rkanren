(load "mk.scm")

(define !-r
  (lambda (gamma e t n)
    (let ([n (add1 n)])
      (fresh (e1 e2 e3 t1 t2) 
        (condr
          [(if (< n 5) 30 1) 
           (== e `(intc ,e1))
           (== t 'int)]
          [(if (< n 5) 30 2) 
           (== e `(+ ,e1 ,e2))
           (== t 'int)
           (!-r gamma e1 'int n)
           (!-r gamma e2 'int n)]
          [(if (< n 5) 30 1) 
           (== e `(var ,e1))
           (lookupo gamma e1 t)]
          [4 
           (== e `(lambda (,e1) ,e2))
           (== t `(-> ,t1 ,t2))
           (!-r `((,e1 . ,t1) . ,gamma) e2 t2 n)]
          [2 
           (== e `(app ,e1 ,e2))  
           (!-r gamma e1 `(-> ,t1 ,t) n)
           (!-r gamma e2 t1 n)])))))

(define !-o
  (lambda (gamma e t) 
    (fresh (e1 e2 e3 t1 t2) 
      (conde
        [(== e `(intc ,e1))
         (== t 'int) ]
        [(== e `(+ ,e1 ,e2))
         (== t 'int)
         (!-o gamma e1 'int)
         (!-o gamma e2 'int)]
        [(== e `(var ,e1))
         (lookupo gamma e1 t)]
        [(== e `(lambda (,e1) ,e2))
         (== t `(-> ,t1 ,t2))
         (!-o `((,e1 . ,t1) . ,gamma) e2 t2)]
        [(== e `(app ,e1 ,e2))  
         (!-o gamma e1 `(-> ,t1 ,t))
         (!-o gamma e2 t1)]))))

(define lookupo
  (lambda (G x t)
    (fresh (rest type y)
      (conde
      ((== `((,x . ,t) . ,rest) G))
      ((== `((,y . ,type) . ,rest) G)
       (=/= x y)
       (lookupo rest x t))))))

