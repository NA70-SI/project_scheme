;; done
(module sim-lane-list racket
  (provide lane-list?
           less-crowded
           )

  (require "sim-lane.rkt")

  
 
  (define (lane-list? lst) ; checks if the input is a valid list of lanes
    (if
     (null? lst) ; checkes whether the input  is null
     #t
     (and
      ;;checks if the 1st lane of the list is a valid lane
      ;;and pass the rest of the lanes to lane-list? for checking sequentially using a recursive call
      (lane? (car lst)) (lane-list? (cdr lst)))))


  
  
  (define (less-crowded sim-lanes) ; gets the least crowded lane from list of lanes 
    (car (sort sim-lanes ; gets the 1st lane after sorting (sorted in an ascending way, least to most crowded)
               (lambda (a b)  ;takes two arguments for comparison with conditiions               
                  (< (lane-length a)(lane-length b)) ; sorts in an ascending order                
                  ))))
  
        
             
             
             
 )       
    