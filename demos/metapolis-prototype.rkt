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
;             overflow: "scroll"
             border: "1px solid black")
    contents))

(define s 10)

(define (place-middle pl)
  (posn-scale 0.5
              (posn-add (place-posn pl)
                        (place-posn2 pl))))

(define (place->step-posn pl)
  (posn (* s (posn-x (place-middle pl)))  
        (* s (posn-y (place-middle pl)))))

(define-syntax-rule (place->step pl stuff ...)
  (step #:x  (posn-x (place->step-posn pl))
        #:y  (posn-y (place->step-posn pl))
        stuff ...
        (rect (* s (place-width pl))  
              (* s (place-height pl))  
              (p (place-name pl)))))

(define-syntax-rule (step-at p stuff ...)
  (step #:x (posn-x p)
        #:y (posn-y p)
        stuff ...))


(define (nav-neighbors r s)
  (define i (index-of r s ))
  (define p (modulo (sub1 i) (length r)))
  (define n (modulo (add1 i) (length r)))

  (~a (list-ref r p)
      " "
      (list-ref r n)))

(define preview-ring
  '())

(define (zoom-pair id p (zoom-ring '()))
  (define preview-id (~a id "-preview"))
  (define zoom-id (~a id "-zoom"))

  (set! preview-ring (cons (string->symbol preview-id) 
                           preview-ring))

  (list
    (place->step p
                 id: preview-id
                 #:z 0
                 #:goto (~a id "-preview")
                 #:key-list "ArrowUp ArrowRight ArrowLeft"
                 #:next-list (thunk (~a zoom-id " "
                                        (nav-neighbors preview-ring 
                                                       (string->symbol preview-id)))))

    (step-at (place->step-posn p)
             id: (~a id "-zoom")
             #:goto (~a id "-zoom")
             #:scale 0.1
             #:key-list (~a "ArrowDown" (if (empty? zoom-ring) 
                                              ""
                                              (~a " ArrowRight ArrowLeft" )))
             #:next-list (~a id "-preview" (if (empty? zoom-ring) 
                                              ""
                                              (~a " " (nav-neighbors zoom-ring
                                                                     (string->symbol zoom-id)))))
             #:z -1000
             (write-img 
               (overlay 
                 (text (~a (place-name p) " stories") 12 'black)
                 (circle 40 'solid 'red))))))


(define (test)
  (define lovelace-zoom-ring
    '(lovelace-zoom lovelace-zoom-times))  

  (define steps 
    (list
      (zoom-pair "babbage" places:babbage-university)
      (zoom-pair "pascal" places:pascal-elementary)
      (zoom-pair "lovelace" places:lovelace-elementary
                 lovelace-zoom-ring)

      (step-at (posn-add (place->step-posn places:lovelace-elementary)
                         (posn 10 0))
               #:scale 0.1
               #:goto "lovelace-zoom-times"
               #:key-list "ArrowRight ArrowLeft"
               #:next-list (nav-neighbors lovelace-zoom-ring 'lovelace-zoom-times)
               id: "lovelace-zoom-times"
               (write-img
                 (circle 20 'solid 'green))) 


      ;Unreachable, but visible

      (place->step places:metapolis
                   id: "main-preview"
                   #:z 0)
      ))

  (list
    ;  site 
    (impress-me
      steps )))

(render (test) #:to "out")
