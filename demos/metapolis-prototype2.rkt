#lang at-exp racket

(require (except-in website/impress site time) 
         (except-in website/bootstrap col site time)  
         (except-in 2htdp/image frame)
         "../main.rkt")

(require metapolis-stories
         stories
         posn)


(define (rect w h . contents)
  (div
    style: (properties 
             width: w
             height: h
             border-radius: "5%"
             border: "1px solid black")
    contents))

(define s 10)

(define (place-id pl)
  (urlify (place-name pl)))

(define (place-middle pl)
  (posn-scale 0.5
              (posn-add (place-posn pl)
                        (place-posn2 pl))))

(define (place->step-posn pl)
  (posn (* s (posn-x (place-middle pl)))  
        (* s (posn-y (place-middle pl)))))

(define (place->level pl)
  (define parent
    (node 
      #:id (place-id pl)
      (posn-x (place->step-posn pl))
      (posn-y (place->step-posn pl))
      (rect (* s (place-width pl))  
            (* s (place-height pl))  
            (p (place-name pl)))))

  (level parent 
         (ring
           (node 
             0 0
             (write-img 
               (circle 20 'outline 'red))))))

(define (test)
  (define steps 
    (ring->steps
      (apply ring
        (map place->level places:all))))

  (impress-me steps))

(render (test) #:to "out")
