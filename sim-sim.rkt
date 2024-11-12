;; Read Module Assignment and Redefinition.
;; The state is made of two
;; event queue and list of lanes
;; 
(module sim-sim racket
  (provide sim-fire-event!           
           ;lanes
           )

  (require "sim-event.rkt")
  (require "sim-event-queue.rkt")
  (require "sim-utils.rkt")
  (require "sim-lane-list.rkt")
  (require "sim-lane.rkt")


  ;; State modifiers 
  (define (sim-fire-event! ev-type user id-lane)
    (let
        ([new-event (event (if (= ev-type LEAVE_EVENT) (+ 5 time) time)
                           ev-type     
                           (cons user id-lane))])
      (begin
        (set! ev-queue
              (sim-add-event ev-queue ;; sort of Priority queue
                             new-event
                             ))
        )))

  ;; IO()
  (define (sim-run! )
    (cond
      [(empty? ev-queue) (void)]
      [else
       (let*
           ([event (first ev-queue)])
         (begin            
           (set! time (event-time event))          
           (sim-process! event)
           (sim-log! time event lanes)
           (set! ev-queue (rest ev-queue))
           (sim-run!)
           )
         )
       ]
      ))

  ;; IO()
  (define (sim-process! event)
    (let
        ([ev-id (event-type event)]
         )
      (cond
        [(= ev-id ENTER_EVENT)
         (let*
             ([user  (event-user event)]
              [lane-l (less-crowded lanes)]
              [index (index-of lanes lane-l)])
           (begin
             (set! lane-l (lane-append! lane-l user))
             (set! lanes (list-set lanes index lane-l))
             ))]
        [(= ev-id CHECKOUT_EVENT)
         (let*
             ([user  (event-user event)]
              [index  (event-lane event)] ;; index, not-lae
              [lane-l (list-ref lanes index)]
              )
           (begin
             (set! lane-l (lane-pop! lane-l ))
             (set! lanes (list-set lanes index lane-l))
             ))]
      
        [(= ev-id LEAVE_EVENT)
         (let*
             ([user  (event-user event)]
              [index  (event-lane event)]
              [lane-l (list-ref lanes index)]
              )
           (begin
             (set! lane-l (lane-bussy-set! lane-l null))
             (set! lanes (list-set lanes index lane-l))
             )
           )]
        )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  (define N (read))
  (define M (read))
  (define time 0)
  
  (define lanes
    (map (lambda (x) (lane x sim-fire-event!))
         (range M)))
  
  (define ev-queue
    (map
     (lambda (x) (event 0 ENTER_EVENT (cons x null)))
     (range N) 
     ))

  (begin
    (sim-run!))
  )