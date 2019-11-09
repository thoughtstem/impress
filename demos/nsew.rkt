#lang racket

(require website/impress)
(require website/bootstrap)
(require "../main.rkt" 
         "./common.rkt")

(define (level-1)
  (ring
    (level
      (node 0 0
            #:id "top"
            (circ-div 'red))
      (level-2))
    (node 200 200
          (circ-div 'red))))

(define (level-2)
  (ring
    (node 0 0
          #:id "nice"
          (circ-div 'orange))

    (level-2-b)))

(define (level-2-b)
  (level
    (node 200 200 
          #:id "broken-parent"
          (circ-div 'yellow))

    (level-3)))

(define (level-3)
  (ring
    (node 0 0
          #:id "broken"
          (circ-div 'lime))
    
    (level
      (node 200 200
            (circ-div 'blue))
      (level-4))))

(define (level-4)
  (ring
    (node 200 200
          (circ-div 'purple))
    
    (node 0 0
          (circ-div 'violet))
    ))

(render-ring 
  (ring (level-1)))  





