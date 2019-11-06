#lang racket

(require "../main.rkt")

(require 
  (except-in 2htdp/image frame)
  (except-in website/impress site time) )

;TODO: y direction is not handled yet in all places (offset-y)
(begin
  (define a (node #:id "a"
                       0 0 
                       (write-img 
                         (circle 20 'outline 'red))))

  (define b (node #:id "b" 
                       200 0 
                       (write-img 
                         (circle 20 'outline 'blue))))


  (define ax (node #:id "ax"
                        0 0 
                        (write-img 
                          (circle 20 'outline 'red))))

  (define ay (node #:id "ay" 
                        100 0 
                        (write-img 
                          (circle 20 'outline 'red))))

  (define ay-1 (node #:id "ay-1"
                        0 0 
                        (write-img 
                          (star 20 'solid 'red))))

  (define bx (node #:id "bx"
                        0 0 
                        (write-img 
                          (circle 20 'outline 'blue))))

  (define by (node #:id "by"
                        100 0 
                        (write-img 
                          (circle 20 'outline 'blue))))


  (define by-1 (node #:id "by-1"
                        0 0 
                        (write-img 
                          (star 20 'solid 'blue))))

  (define ra (ring ax ;Putting a level as the first thing in the ring causes an error
                        (level ay
                               (ring ay-1))))
  (define rb (ring bx (level by 
                                  (ring by-1))))

  (define la (level a ra))
  (define lb (level b rb))

  (define r-top (ring la lb))

  (define steps (ring->steps r-top)))


(define (test)
  (impress-me steps )) 

(render (test) #:to "out") 
