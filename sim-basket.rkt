(module sim-basket racket
  (provide basket
           basket?
           basket-id
           basket-items)

  ;; P: id number
  (define (basket id )
    (cons id 5))

  ;; P : true
  (define (basket? val)
    (and
     (pair? val)
     (number? (car val))))

  ;; P : val as basket
  (define (basket-id val)
    (car val))

  ;; P : val as basket
  (define (basket-items val)
    (cdr val))

  ;; P :
  (define (basket->string val)
    (format "~a~n" (basket-id val)))
  )
    

  
