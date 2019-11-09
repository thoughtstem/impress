#lang racket

(require website/impress)
(require website/bootstrap)
(require "../main.rkt" 
         "./common.rkt")

(define (three-tree
          #:a (a #f)
          #:b (b #f)
          #:c (c #f))

  (define middle
    (node 0 0
          (circ-div 'orange)))

  (define left
    (node -150 0
          (circ-div 'red)))

  (define right
    (node 150 0
          (circ-div 'yellow)))

  (ring
    (if b
      (level middle b)
      middle)
    
    (if a
      (level left a)
      left)

    (if c 
      (level right c)
      right)))

(define (hi-node)
  (node 0 0
        (h1 "HI!")))

(define r
  (three-tree
    #:a (three-tree)
    #:b (three-tree
          #:a (three-tree) 
          #:b (three-tree)
          #:c (three-tree))
    #:c (three-tree)))

(with-depth 3
            (render-ring r))


