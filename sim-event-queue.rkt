;; Done
(module sim-event-queue racket
  (provide
   sim-add-event
   )

  (require "sim-event.rkt")

  ;; This is a sort of "priority queue"
  ;; in O(nlog(n))
(define (sim-add-event sim-event-queue ev) ;returns a list of events in ascending order based on conditions
  
  ;sorts the merged sim-event-queue(current event) and ev(new list) based on time & user
  (sort (append sim-event-queue (list ev))
        (lambda (a b)       ;function with arguments for comparison
          (if (= (event-time a) (event-time b)); checks if the event times are equal 
              (< (event-user a) (event-user b)) ;if so, sort by event user 
              (< (event-time a) (event-time b))))));else, sort by event time



  
          

  
)