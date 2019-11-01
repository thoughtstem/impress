#lang at-exp racket

(provide impress-me
         yt)

(require website/impress 
         (only-in website/bootstrap
                  include-bootstrap-js
                  include-bootstrap-css)
         (except-in 2htdp/image frame))

(define (impress-me . steps)
  (impress-site #:transition-duration 200
    #:head (head)
    steps
    (include-bootstrap-js)))


(define (yt i)
  (iframe 
    src: (~a "https://www.youtube.com/embed/" i)
    width: "560" 
    height: "315" 
    'frameborder: "0" 
    'allowfullscreen: "0"))  

(define (head)
  (list
    (include-bootstrap-css)  
    #;(link href: "https://fonts.googleapis.com/css?family=Open+Sans:regular,semibold,italic,italicsemibold|PT+Sans:400,700,400italic,700italic|PT+Serif:400,700,400italic,700italic" 'rel: "stylesheet")
    #;(link href: "https://impress.js.org/css/impress-demo.css" 'rel: "stylesheet")
    ))



