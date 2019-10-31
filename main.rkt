#lang at-exp racket

(require (except-in website/impress site time) 
         (except-in website/bootstrap col site time)  
         (except-in 2htdp/image frame)
         "./util.rkt")

(require metapolis-stories
         stories)

;What abstractions?

;Things within things...

(define (rect w h color)
  (div
    style: (properties 
             width: w
             height: h
             background-color: color)))


(define (test)
  (list
    site 
    (impress-me
      (step #:x 0 #:y 0 #:scale 10
            id: "vantage")

      (step #:x 0 #:y 0
            id: "green"
            (div
              style:
              (properties
                width: 1000)
              (container
                (render-place
                  places:lovelace-elementary)))))))

(render 
  (parameterize ([all-stories stories:all])
    (test)) 
  #:to "out")
