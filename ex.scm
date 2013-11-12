(define gen-expo
  (lambda (out)
    (fresh (x e1 e2 e3 body)
      (condr
        (10 (== x out))
        (2 (== out `(lambda (,x) ,body))
           (gen-expo body))
        (4 (== out `(if ,e1 ,e2 ,e3))
           (gen-expo e1)
           (gen-expo e2)
           (gen-expo e3))
        (1 (== out `(,e1 ,e2)))))))
