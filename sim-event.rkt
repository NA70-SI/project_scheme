(module event racket
  (provide event
           event?
           event-time
           event-type
           event-user
           event-lane
           event-params
           ENTER_EVENT
           CHECKOUT_EVENT
           LEAVE_EVENT
           event->string
           )

  (define ENTER_EVENT 1)
  (define CHECKOUT_EVENT 2)
  (define LEAVE_EVENT 3)

  (define event-dict
    (list (cons ENTER_EVENT "ENTER_EVENT")
          (cons CHECKOUT_EVENT "CHECKOUT_EVENT")
          (cons LEAVE_EVENT "LEAVE_EVENT")))
          

  ;; constructor.
  ;; ( time . ( id . (user . id-lane)))
(define (event time id params)
  (cons time(cons id params)); create the format of event(reformats pair)
  )

  ;; P: True
(define (event? val) ;checkes if the val is a an event (returns #t if so)
    (and (pair? val) ; must be a pair
       (integer? (car val)) ; checks if the 1st element (time) is an integer
       (>= (car val) 0)     ; checks if the 1st element is greater of qual to 0              
       (pair? (cdr val))    ; the 2nd element must be a pair
       (member (car (cdr val)) '(1 2 3)) ;the second element must be either 1,2 or 3
       (pair? (cdr (cdr val)))   ;3rd element must be a pair
       (and (integer? (car (cdr(cdr val))))(>= (car (cdr(cdr val))) 0)) ;1st part of 3rd element must be an integer
       (and (integer? (cdr (cdr(cdr val))))(>= (cdr (cdr(cdr val))) 0)))) ;2nd part of 3rd element must be an integer
 


    
  ;; P: valid event
  (define (event-time ev)
    (car ev)); gets the 1st element of ev (time)

  ;; P: valid event
  (define (event-type ev)
    (cadr ev)) ; gets the 1st item of 2nd pair (types of event)

  

  ;; P: event-params
  (define (event-params ev)
    (cddr ev)) ;gets the 2nd item of 2nd element as a pair(customer & lane ID)

  (define (event-user ev)
    (car (cddr ev))) ;returns the customer ID engaged in this event (car of cdr of cdr)
  
  (define (event-lane ev)
    (cdr (cddr ev))); ;returns the lane ID related to the event (cdr of cdr of cdr)



  (require racket/dict)
  (define (event->string ev)
    (format "~a"
            (dict-ref event-dict (event-type ev))
            ))
  )