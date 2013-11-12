;;; This file was generated by writeminikanren.pl
;;; Generated at 2007-10-25 15:24:42

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (errorf 'test-check
               "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
               'tested-expression expected produced)))))))

(print-gensym 'pretty/suffix)
(define-record stream (rank proc))

(define-syntax lambda-goal-subst
  (syntax-rules ()
    ((_ (p) e) (lambda (p) e))
    ((_ (p : r s c*) e)
     (lambda (p)
       (let ((r (subst-rank p))
             (s (subst-alist p))
             (c* (subst-diseq p))
             )
         e)))))

;(define-syntax lambdaf@
;  (syntax-rules ()
;    ((_ () e) (lambda () e))))

(define-syntax lambdaf@
  (syntax-rules ()
    ((_ () r e) (make-stream r (lambda () e)))))

;(define-syntax thunk
;  (syntax-rules () ((_ e) (lambdaf@ () e))))

(define rhs (lambda (x) (cdr x)))

(define lhs (lambda (x) (car x)))

(define-syntax var ;: a -> var
  (syntax-rules ()
    ((_ x) (vector x))))

(define-syntax var? ;: a -> bool
  (syntax-rules ()
    ((_ x) (vector? x))))

;------------------------------------------------------------------------------
; Thunk Definitions for Streams
;------------------------------------------------------------------------------
(define-syntax thunk
  (syntax-rules () ((_ r e) (make-stream r (lambda () e)))))

(define stream-incr-rank
  (lambda (stream)
    (make-stream (add1 (stream-rank stream)) (stream-proc stream))))

(define invoke
  (lambda (stream)
    ;; (printf "Thunk: ~s~n" (stream-rank stream))
    ((stream-proc stream))))

;; If we are getting the rank of a stream, return that.
;; Otherwise, return -1 because we have failed and want to do it NOW.
(define get-rank
  (lambda (a-inf)
    (cond
      [(stream? a-inf) (stream-rank a-inf)]
      [(subst? a-inf)  (subst-rank a-inf)]
      [else -1])))

(set! debug-var #f)

(define debug-printf
  (lambda ls
    (if debug-var (apply printf ls) (void))))

;------------------------------------------------------------------------------
; Substitution Definitions
;------------------------------------------------------------------------------
(define-record subst (rank alist diseq))

(set! empty-a (make-subst 0 '() '())) ;: subst

(define lookup-s ;: var -> subst -> (var, val)
  (lambda (u S)
    (assq u (subst-alist S))))

(define subst-incr-rank ;: subst -> subst
  (lambda (s)
    (make-subst (add1 (subst-rank s)) (subst-alist s) (subst-diseq s))))

(define subst-add-rank ;: subst -> subst
  (lambda (s r)
    (make-subst (+ (subst-rank s) r) (subst-alist s) (subst-diseq s))))

(define ext-s ;: var -> val -> subst -> subst
  (lambda (x v s)
    (make-subst (subst-rank s) (cons `(,x . ,v) (subst-alist s)) (subst-diseq s))))

(define ext-s-check ;: var -> val -> subst -> (Either subst #f)
  (lambda (x v s)
    (cond
      ((occurs-check x v s) #f)
      (else (ext-s x v s)))))

(define occurs-check ;: var -> var-> subst -> bool
  (lambda (x y s)
    (let ((y (walk y s)))
      (cond
        ((var? y) (eq? y x))
        ((pair? y)
         (or
           (occurs-check x (car y) s)
           (occurs-check x (cdr y) s)))
        (else #f)))))

(define-syntax size-s ;: subst -> int
  (syntax-rules ()
    ((_ x) (length (subst-alist  x)))))


;------------------------------------------------------------------------------
; case-inf
;------------------------------------------------------------------------------

(define-syntax mzero
  (syntax-rules () ((_) #f)))

(define incr-rank
  (lambda (a-inf)
    (cond
      [(stream? a-inf) (stream-incr-rank a-inf)]
      [(subst? a-inf)  (subst-incr-rank a-inf)]
      [(pair? a-inf)   (cons (incr-rank (car a-inf)) (incr-rank (cdr a-inf)))]
      [else a-inf])))

(define-syntax case-inf
  (syntax-rules ()
    ((_ e (() no-ans)
          ((stream^) more-stream)
          ((ans^) one-ans)
          ((ans stream) ans-and-stream))
     (let ([a-inf e])
       (cond
         [(not a-inf) (incr-rank no-ans)]
         [(stream? a-inf)
          (let ([stream^ a-inf]) (incr-rank more-stream))]
         [(not (and (pair? a-inf) (stream? (cdr a-inf))))
          (let ([ans^ a-inf]) (incr-rank one-ans))]
         [else
           (let ([ans (car a-inf)]
                 [stream (cdr a-inf)])
             (incr-rank ans-and-stream))])))))

;------------------------------------------------------------------------------
; choise, mzero, mplus, and other deep guts
;------------------------------------------------------------------------------
(define-syntax choice
  (syntax-rules () ((_ a f) (cons a f))))

(define mplus
  (lambda (a-inf f)
    (case-inf a-inf
      (() (invoke f))
      ((f^) (let ([f^-rank (get-rank f^)]
                  [f-rank  (get-rank f)])
              (if (< f^-rank f-rank)
                  (thunk f^-rank (mplus (invoke f^) f))
                  (thunk f-rank  (mplus (invoke f)  f^)))))
      ((a) (choice a f))
      ((a f^) (choice a
                      (let ([f^-rank (get-rank f^)]
                            [f-rank  (get-rank f)])
                        (if (< f^-rank f-rank)
                          (lambdaf@ () f^-rank (mplus (invoke f^) f))
                          (lambdaf@ () f-rank  (mplus (invoke f)  f^)))))))))

(define apply-min
  (lambda (ls)
    (apply min ls)))

(define-syntax mplus*
  (syntax-rules ()
      ((_ e) e)
      ((_ e0 e ...)
        (let ([min-rank (apply-min (map get-rank `(,e ...)))]
              [e0-rank (get-rank e0)])
          (if (< e0-rank min-rank)
            (mplus e0 (lambdaf@ () min-rank (mplus* e ...)))
            (mplus (lambdaf@ () min-rank (mplus* e ...)) (lambdaf@ () e0-rank e0)))))))

;------------------------------------------------------------------------------
; unit and bind---the nondeterminism monad, after a fashion
;------------------------------------------------------------------------------

(define-syntax unit
  (syntax-rules () ((_ a) a)))

(define bind
  (lambda (a-inf g)
    (case-inf a-inf
      (() (mzero))
      ((f) (thunk (get-rank f) (bind (invoke f) g)))
      ((a) (g a))
      ((a f) (mplus (g a)
                    (lambdaf@ () (get-rank f) (bind (invoke f) g)))))))

(define-syntax bind*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...) (bind* (bind e g0) g ...))))

;------------------------------------------------------------------------------
; miniKanren-isms: run, run*, fresh, ==, and take (which run uses)
;------------------------------------------------------------------------------

(set! empty-f  (lambdaf@ () 0 (mzero)))

(define-syntax run
  (syntax-rules ()
    ((_ n (x) g0 g ...)
     (take n
       (lambdaf@ () 0
         ((fresh (x) g0 g ...
            (lambda-goal-subst (a)
               (choice ((reify x) a) empty-f)))
          empty-a))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x) g ...) (run #f (x) g ...))))

(define =/=
  (lambda (u v)
    (lambda-goal-subst (a : r s c*)
      (cond
        ((unify u v a) =>
         (lambda (a0)
           (let ((s0 (subst-alist a0)))
             (cond
               ((eq? s0 s) (mzero))
               (else
                (let ((p* (list (prefix-s s0 a))))
                  (let ((c* (append p* c*)))
                    (unit (make-subst r s c*)))))))))
        (else (unit a))))))

(define ==
  (lambda (u v)
    (lambda-goal-subst (a : r s c*)
      (cond
        ((unify u v a) =>
         (lambda (a0)
           (let ((s0 (subst-alist a0)))
             (cond
               ((eq? s0 s) (unit a))
               ((verify-c* c* (make-subst r s0 c*)) =>
                (lambda (c*)
                  (unit (make-subst r s0 c*))))
               (else (mzero))))))
        (else (mzero))))))

(define verify-c*
  (lambda (c* s)
    (cond
      ((null? c*) '())
      ((verify-c* (cdr c*) s) =>
       (lambda (c0*)
         (let ((c (car c*)))
           (cond
             ((verify-c*-aux c c0* s))
             (else (mzero))))))
      (else #f))))

(define verify-c*-aux
  (lambda (c c0* a)
    (cond
      ((unify* c a) =>
       (lambda (a0)         
         (let ((s0 (subst-alist a0)))           
           (and (not (eq? s0 (subst-alist a)))
                (cons (prefix-s s0 a) c0*)))))
      (else c0*))))

(define prefix-s
  (lambda (s0 a)
    (cond
      ((eq? s0 (subst-alist a)) '())
      (else (cons (car s0)
              (prefix-s (cdr s0) a))))))

(define unify*
  (lambda (c s)
    (unify (map lhs c) (map rhs c) s)))

(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambda-goal-subst (a : r s c*)
       (thunk r
         (let ((x (var 'x)) ...)
           (bind* (g0 a) g ...)))))))

(define take
  (lambda (n f)
    (if (and n (zero? n))
      '()
      (case-inf (invoke f)
        (() '())
        ((f) (take n f))
        ((a) (cons a '()))
        ((a f)
         (cons a
           (take (and n (- n 1)) f)))))))

;------------------------------------------------------------------------------
; miniKanren-isms: cond*, conde, conda, condu
;------------------------------------------------------------------------------

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambda-goal-subst (a : r s c*)
       (thunk r
         (let ((a (subst-incr-rank a)))
          (mplus*
           (bind* (g0 a) g ...)
           (bind* (g1 a) g^ ...) ...)))))))

(define-syntax condr
  (syntax-rules ()
    ((_ (p0 g0 g ...) (p1 g1 g^ ...) ...)
     (lambda-goal-subst (a : r s c*)
       (thunk r
         (let ((a (subst-incr-rank a)))
          (mplus*
           (bind* (g0 (subst-add-rank a p0)) g ...)
           (bind* (g1 (subst-add-rank a p1)) g^ ...) ...)))))))


(define-syntax conda
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambda-goal-subst (a : r s c*)
       (thunk r
         (let ((s (subst-incr-rank a)))
           (ifa ((g0 a) g ...)
                ((g1 a) g^ ...) ...)))))))

(define-syntax ifa
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((a-inf e))
       (case-inf a-inf
         (() (ifa b ...))
         ((f) (thunk (add1 (stream-rank f)) (loop (invoke f))))
         ((a) (bind* a-inf g ...))
         ((a f) (bind* a-inf g ...)))))))

(define-syntax condu
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambda-goal-subst (a : r s c*)
       (thunk r
         (let ((a (subst-incr-rank a)))
           (ifu ((g0 a) g ...)
                ((g1 a) g^ ...) ...)))))))

(define-syntax ifu
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((a-inf e))
       (case-inf a-inf
         (() (ifu b ...))
         ((f) (thunk (add1 (stream-rank f)) (loop (invoke f))))
         ((a) (bind* a-inf g ...))
         ((a f) (bind* (unit a) g ...)))))))

(define-syntax project
  (syntax-rules ()
    ((_ (x ...) g g* ...)
     (lambda-goal-subst (a)
       (let ((x (walk* x a)) ...)
         ((fresh () g g* ...) a))))))

(define succeed (== #f #f))

(define fail (== #f #t))

(define onceo
  (lambda (g)
    (condu
      (g succeed)
      ((== #f #f) fail))))

;------------------------------------------------------------------------------
; Walking and Reification Code
;------------------------------------------------------------------------------

(define unify
  (lambda (u v s)
    (let ((u (walk u s))
          (v (walk v s)))
      (cond
        ((eq? u v) s)
        ((var? u) (ext-s-check u v s))
        ((var? v) (ext-s-check v u s))
        ((and (pair? u) (pair? v))
         (let ((s (unify
                    (car u) (car v) s)))
           (and s (unify
                    (cdr u) (cdr v) s))))
        ((equal? u v) s)
        (else #f)))))

(define walk
  (lambda (u S)
    (cond
      ((and (var? u) (lookup-s u S)) =>
       (lambda (pr) (walk (rhs pr) S)))
      (else u))))

(define walk*
  (lambda (w s)
    (let ((v (walk w s)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons
           (walk* (car v) s)
           (walk* (cdr v) s)))
        (else v)))))

;; (define reify-s
;;   (lambda (v s)
;;     (let ((v (walk v s)))
;;       (cond
;;         ((var? v)
;;          (ext-s v (reify-name (gensym)) s))
;;         ((pair? v) (reify-s (cdr v)
;;                      (reify-s (car v) s)))
;;         (else s)))))
;; 
;; (define reify-name
;;   (lambda (n) n))

(define reify-s
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        ((var? v)
         (ext-s v (reify-name (size-s s)) s))
        ((pair? v) (reify-s (cdr v)
                     (reify-s (car v) s)))
        (else s)))))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(define reify
  (lambda (v)
    (lambda-goal-subst (a : r s c*)
      (let ((v (walk* v a)))
        (let ((r (reify-s v empty-a))) 
          (let ((c* (remp
                     (lambda (c)
                       (anyvar? c r))
                     c*)))
            (reify-aux r v (rem-subsumed c*))))))))

(define anyvar?
  (lambda (c r)
    (cond
      ((pair? c)
       (or (anyvar? (car c) r)
           (anyvar? (cdr c) r)))
      (else (and (var? c) (var? (walk c r)))))))

(define rem-subsumed
  (lambda (c*)
    (let rem-subsumed ((c* c*) (c^* '()))
      (cond
        ((null? c*) c^*)
        ((let ((car-c*-subst (make-subst 0 (car c*) '())))
           (or (subsumed? car-c*-subst (cdr c*))
               (subsumed? car-c*-subst c^*)))
         (rem-subsumed (cdr c*) c^*))
        (else (rem-subsumed (cdr c*)
                (cons (car c*) c^*)))))))

(define subsumed?
  (lambda (c c*)
    (cond
      ((null? c*) #f)
      (else
        (let ((c^ (unify* (car c*) c)))
          (or
            (and c^ (eq? (subst-alist c^)
                         (subst-alist c)))
            (subsumed? c (cdr c*))))))))

(define reify-aux
  (lambda (r v c*)
    (let ((v (walk* v r))
          (c* (walk* c* r)))
      (let ((c* (drop-dot-D (sort-D c*))))  
        (cond
          ((null? c*) v)
          (else `(,v (=/= . ,c*) . ())))))))

(define sort-D
  (lambda (D)
    (sort lex<=? (map sort-d D))))

(define sort-d
  (lambda (d)
    (sort
      (lambda (x y)
        (lex<=? (car x) (car y)))
      (map sort-pr d))))

(define lex<=?
  (lambda (x y)
    (cond
      ((vector? x) #t)
      ((vector? y) #f)
      ((port? x) #t)
      ((port? y) #f)
      ((procedure? x) #t)
      ((procedure? y) #f)
      ((boolean? x)
       (cond
         ((boolean? y) (or (not x) (eq? x y)))
         (else #t)))
      ((boolean? y) #f)
      ((null? x) #t)
      ((null? y) #f)
      ((char? x)
       (cond
         ((char? y) (char<=? x y))
         (else #t)))
      ((char? y) #f)
      ((number? x)
       (cond
         ((number? y) (<= x y))
         (else #t)))
      ((number? y) #f)
      ((string? x)
       (cond
         ((string? y) (string<=? x y))
         (else #t)))
      ((string? y) #f)
      ((symbol? x)
       (cond
         ((symbol? y)
          (string<=? (symbol->string x)
                     (symbol->string y)))
         (else #t)))
      ((symbol? y) #f)
      ((pair? x)
       (cond
         ((pair? y)
          (cond          
            ((equal? (car x) (car y))
             (lex<=? (cdr x) (cdr y)))
            (else (lex<=? (car x) (car y)))))))
      ((pair? y) #f)
      (else #t))))         

(define sort-pr
  (lambda (pr)
    (let ((l (lhs pr))
          (r (rhs pr)))
      (cond
        ((lex<-reified-name? r) pr)
        ((lex<=? r l) `(,r . ,l))
        (else pr)))))

(define lex<-reified-name?
  (lambda (r)
    (char<?
      (string-ref (datum->string r) 0)
      (string-ref "_" 0))))

(define datum->string
  (lambda (x)
    (call-with-string-output-port
      (lambda (p) (display x p)))))

(define drop-dot-D
  (lambda (D)
    (map
     (lambda (X)
       (map (lambda (t)
              (let ((a (lhs t))
                    (d (rhs t)))
                `(,a ,d)))
            X))
     D)))

;; (trace bind)
;; (trace mplus)

(define test
  (lambda (e n)
    (fresh (a b)
      (condr 
        ((if (< n 1) 10 1) (== e '(x)))
        (2 (== e `(b . ,a)) (test a (add1 n)))
        (4 (== e `(a . ,b)) (test b (add1 n)))))))

;; (load "==-tests.ss")
;; (load "mktests.scm")
;; (load "disequality-tests.ss")
