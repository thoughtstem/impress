#lang at-exp racket

(provide impress-me
         yt
         
         (rename-out 
           [make-node node]
           [make-ring ring]
           [make-level level])
         ring->steps
         level-depth
         with-depth
         on-node-visit
         string->node-id)

(require website/impress 
         (only-in website/bootstrap
                  include-bootstrap-js
                  include-bootstrap-css)
         (except-in 2htdp/image frame))

;Rings are like sibling you can navigate between (currently with right and left, later perhaps with cardinal directions -- like a net)
;From anywhere in a ring, you can back up a level to the ring's parent
;Nodes in rings may be parents of other rings, and so on.  Hopefully allowing for infinite organizational freedom.  At each zoom in level, you have more space...

(struct node (id x y content) #:transparent)
(struct ring (nodes) #:transparent)
(struct level (node ring))

(define (string->node-id s)
  (string-replace #:all? #t
    (string-replace #:all? #t
                    s "'" "")
    " " "-"))

(define (make-node #:id (id (~a (gensym 'id))) x y content)
  (node (string-replace id "'" "") x y content))

(define (make-ring . ns)
  (ring (flatten ns)))

(define (make-level parent children)
  (level parent children))

(define (ring-neighbors r n)
  (define ns (map (lambda (n)
                    (if (level? n)
                      (level-node n)
                      n)) 
                  (ring-nodes r)))
  (define i (index-of ns n))
  (define prev (modulo (sub1 i) (length ns)))
  (define next (modulo (add1 i) (length ns)))

  (list (list-ref ns prev) 
        (list-ref ns next)))

(define (ring->next-list r n)
  (string-join
    (map node-id (ring-neighbors r n))
    " "))  

(define current-ring          (make-parameter #f))
(define current-parent-stack  (make-parameter '()))
(define current-child         (make-parameter #f))

(define level-depth (make-parameter 3))
(define-syntax-rule (with-depth n stuff ...)
  (parameterize ([level-depth n])
    stuff ...))

(define (node-scale n)
  (exact->inexact 
    (expt (level-depth) (- (length (current-parent-stack))))))

(define (current-ring-neighbors n)
  (if (not (current-ring))
    '()  
    (ring-neighbors (current-ring) n)))

(define (landing-node? n)
  (and (not (empty? (current-parent-stack)))
       (not (empty? (ring-nodes (current-ring))))
       (eq? n (first (ring-nodes (current-ring))))))

(define (current-level-neighbors n)
  (filter-not void?
              (list
                (when (current-child)
                  (current-child))
                (when (landing-node? n)
                  (first (current-parent-stack))))))

(define (offset-x n)
  (define (adj x i)
    (* x (expt (level-depth) (- i))))

  (if (empty? (current-parent-stack))
    0
    (apply + 
           (map adj 
                (map node-x (current-parent-stack))
                (reverse (range (length (current-parent-stack)))) ))))

(define (offset-y n)
  (define (adj y i)
    (* y (expt (level-depth) (- i))))

  (if (empty? (current-parent-stack))
    0
    (apply + 
           (map adj 
                (map node-y (current-parent-stack))
                (reverse (range (length (current-parent-stack))))))))

(define (entrance-node n)
  (define neighbors (current-level-neighbors n))
  (if (not (empty? neighbors))
    (first neighbors)
    #f))


(define (node->step n)
  (cond
    [(ring? n)  (ring->steps n)]
    [(level? n) (level->steps n)]
    [else (step #:x (+ (* (node-x n) 
                          (node-scale n))
                       (offset-x n))
                #:y (+ (* (node-y n)
                          (node-scale n))
                       (offset-y n))
                #:z (* -1000 (length (current-parent-stack)))
                #:scale (node-scale n)  
                id: (node-id n)
                'onClick: (if (entrance-node n) 
                            (~a "if(window.location.href.includes('#/" (node-id n) "')){setTimeout(() => impress().goto('" (node-id (entrance-node n)) "'), 1)}")
                            "")
                (node-content n))]))

(define (ring->steps r)
  (parameterize ([current-ring r])
    (map node->step 
         (ring-nodes r))))

(define (level->steps l)
  (define n (level-node l))
  (define r (level-ring l))

  (define n-step
    (parameterize ([current-child (first (ring-nodes r))])
      (node->step n)))

  (define r-steps
    (parameterize ([current-parent-stack (cons n (current-parent-stack))]) 
      (ring->steps r)))

  (cons n-step r-steps))


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


(define (on-node-visit . some-js-strings)
  (define combined (string-join some-js-strings))
    @script/inline{
      var rootElement = document.getElementById( "impress" );
      rootElement.addEventListener( "impress:stepenter", function(event) {
        var currentStep = event.target;

        @combined
      }); 

    }
  )
