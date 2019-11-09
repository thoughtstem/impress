#lang racket

(provide render-ring
         circ-div
         hi-node)

(require website/impress)
(require website/bootstrap)
(require "../main.rkt")

(define (circ-div c)
  (div
    style: (properties
             border-radius: "50%"
             width: 50
             height: 50
             'opacity: 0.5
             background-color: c)))

(define (hi-node)
  (node 0 0
        (h1 "HI!")))

(define (render-ring r)
  (render 
    (impress-site #:transition-duration 300
      (debug-impress
        (ring->steps r)))
    #:to "out"))
