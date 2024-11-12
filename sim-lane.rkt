(module sim-lane racket
  (provide lane
           lane?
           lane-id
           lane-user
           lane-bussy?
           lane->string
           lane-length
           lane-pop
           lane-queue
           ; These routines fire events
           lane-append!
           lane-pop!
           lane-bussy-set!
           )

  (require racket/match)
  (define sim-fire-event! null) ; hack to passed procedure


  
  ;; A lane (id . (user . list) 
  (define (lane id sym)
    (set! sim-fire-event! sym) ;; dirty hack
    (cons id (cons null null)))

  
  (define (lane-length ln)
    ;;checks the length of the value of last item of 2nd pair(cdr of cdr) of ln
    (length(cddr ln))) 

  ;
  (define (lane? val) ; checks if the input is valid lane
    (and (pair? val) ;checkes whether the input is pair and if it meets other conditions
         ;;checks if the 1st value of input is number
         (number? (car val))
         ;;checks if the rest is a pair or not
         (pair? (cdr val))
         ;;either the 1st value of 2nd pair is a number or the 2nd pair is null
         (or (number? (car (cdr val))) (null? (cadr val)))
         ;;or  the last vale of 2nd pair is a list or null
         (or (list? (cddr val)) (null? (cddr val)))))



  (define (lane-bussy? val); checks if the till is busy or not
    (if (null? (cadr val)) 
        #f   ; returns false is the till is not busy
        #t)) ; true if the till is not null
    
  

  (define (lane-bussy-set ln user)
    (match ln
      [(cons id (cons _ q))
       (cons id (cons user q))]))


  (define (lane-id lane)
    (car lane)) ;returns the ID of a lane 

  (define (lane-user val)
    (car (cdr val))) ; returns the 1st value(ID of the customer) of 2nd pair 

  (define (lane-queue val)
    (cddr val)) ; returns the last value (list of customers) of the 2nd pair

  ;; Free
  (define (lane->string ln)
    (let
        ([lane-qu-str (apply
                       string-append 
                       (map
                        (lambda (x)(format "~a:" x))
                        (lane-queue ln)))]
          )
      (format "L(~a)[~a]|<=~a"
              (lane-id ln)
              (~a
               (if (lane-bussy? ln) (lane-user ln) "None" ) #:min-width 4)
              lane-qu-str)))

  ;; Pure
  ;; Free
  (define (lane-append ln user)
    (match ln
      [(cons id (cons u q))
       (cons id (cons u (append q (list user))))]))

  ;; Free
  (define (lane-pop ln)
    (match ln
      [(cons id (cons u (cons q qs)))
       (values q
               (cons id (cons u qs)))]))


  ; IO() fires an event
  (define (lane-bussy-set! ln user)
    (let
        ([new-lane (lane-bussy-set ln user)])
      (begin       
        (if (and
             (> (lane-length new-lane) 0)  
             (not (lane-bussy? new-lane))) ;; till is free
            (begin
              (sim-fire-event! 2(first (lane-queue new-lane)) (lane-id new-lane))) ;; CHECKOUT
            (void))
        )          
      new-lane))

  ;; IO () fires an event
  (define (lane-append! ln user)
    (let
        ([new-lane (lane-append ln user)])
      (begin
        (if (and
             (= (lane-length new-lane) 1)  ;; i'm the only 
             (not (lane-bussy? new-lane))) ;; till is free
            (sim-fire-event! 2 user (lane-id new-lane)) ;; CHECKOUT
            (void))
        )          
      new-lane))
         

  ;; IO() fires an event
  (define (lane-pop! ln )
    (let*-values
        ([(user new-lane) (lane-pop ln )]
         [ (new-la2) (lane-bussy-set new-lane user)]
         )
      (begin
        (sim-fire-event! 3 user (lane-id new-la2)) ;; CHECKOUT
        new-la2)))



  
    
  )