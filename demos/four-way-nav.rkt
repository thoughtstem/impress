#lang at-exp racket

(require website/impress (except-in 2htdp/image frame))

;TODO: Put in iframe
;Embedded videos?

(define (impress-test)
  (impress-site
    #:head (head)
    (step #:x 0 #:y 0 #:scale 3
          #:key-list "ArrowLeft ArrowRight ArrowUp ArrowDown Tab" 
          #:next-list "step-2 step-3 step-4 step-5 step-6")

    (step #:x -1000 #:y 0
          class: "slide"
          #:goto "step-1"
          (list
            (h1 "Video West")
            (yt "0bodeyCThJM")))

    (step #:x 1000 #:y 0
          #:goto "step-1"
          (list
            (h1 "Video East")
            (yt "ZgvLTS2zJdo")))

    (step #:y -1000 #:x 0
          class: "slide"
          #:goto "step-1"
          (list
            (h1 "Video North")
            (yt "SI9dBcTjHvk")))

    (step #:y 1000 #:x 0
          #:goto "step-1"
          (list
            (h1 "Video South")
            (yt "JxpObvUlTY")))
    
    (step #:y 0 #:x 0 #:z 10000 #:scale 5
          #:goto "step-1"
          #:key-list "ArrowLeft ArrowRight ArrowUp ArrowDown Tab" 
          #:next-list "step-2 step-3 step-4 step-5 step-1" ) ))

(define (yt i)
  (iframe 
    src: (~a "https://www.youtube.com/embed/" i)
    width: "560" 
    height: "315" 
    'frameborder: "0" 
    'allowfullscreen: "0"))  

(define (head)
  (list
    (link href: "https://fonts.googleapis.com/css?family=Open+Sans:regular,semibold,italic,italicsemibold|PT+Sans:400,700,400italic,700italic|PT+Serif:400,700,400italic,700italic" 'rel: "stylesheet")
    (link href: "https://impress.js.org/css/impress-demo.css" 'rel: "stylesheet")))



