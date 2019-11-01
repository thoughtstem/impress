#lang at-exp racket

(require (except-in website/impress site time) 
         (except-in website/bootstrap col site time)  
         (except-in 2htdp/image frame)
         "./util.rkt")

(require ;metapolis-stories
         stories)

;What abstractions?
;  Observation net?  Move cardinally above a more chaotic/textured space?

;Things within things...

(define (rect w h color . contents)
  (div
    style: (properties 
             width: w
             height: h
             border-radius: "50%"
             border: "10px solid black")
    contents))


(define (test)
  (list
  ;  site 
    (impress-me

      (step #:x 0 #:y 0 #:scale 10
            id: "vantage-1"
            #:key-list "ArrowRight Enter"
            #:next-list "vantage-2  cluster")
      (step #:x 10000 #:y 0 #:scale 10
            id: "vantage-2"
            #:key-list "ArrowLeft"
            #:next-list "vantage-1")

      (step #:x 0 #:y 0
            id: "cluster" 
            (rect 1000 1000 "black"))

      (step #:x 0 #:y 0 #:scale 0.1
            (rect 1000 1000 "red"))

      (step #:x 0 #:y 0 #:scale 0.01
            (iframe src: "http://localhost:8000/index.html"
                    style:
                    (properties
                      width: "100vw"
                      height: "100vh")))
      )))

(render 
  (test)
  #;
  (parameterize ([all-stories stories:all])
    (test)) 
  #:to "out")
