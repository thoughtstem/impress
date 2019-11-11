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
(struct level (node ring) #:transparent)

(define (string->node-id s)
  (string-replace #:all? #t
    (string-replace #:all? #t
                    s "'" "")
    " " "-"))

(define (get-id n)
  (cond 
    [(level? n) (node-id (level-node n))]
    [(node? n) (node-id n)]
    [(not n) ""]))

(define (make-node #:id (id (~a (gensym 'id))) x y content)
  (node (string-replace id "'" "") x y content))

(define/contract (make-ring . ns)
  (->* () () #:rest (listof (or/c level? node? ring? list?)) ring?)
  (ring (ring-flatten ns)))


(define (ring-flatten ns)
  (define (de-ring n)
    (if (ring? n)
      (ring-nodes n)
      n))
  (flatten (map de-ring (flatten ns))))

(define/contract (make-level parent children)
  (-> node? 
      (or/c node? level? ring?) 
      level?)
  (level parent 
         (cond 
           [(or (node? children)
                (level? children)) 
            (make-ring children)]
           [else children])))

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


(define current-ring          (make-parameter #f))
(define current-parent-stack  (make-parameter '()))
(define current-child         (make-parameter #f))

(define level-depth (make-parameter 3))
(define-syntax-rule (with-depth n stuff ...)
  (parameterize ([level-depth n])
    stuff ...))


(define (current-ring-neighbors n)
  (if (not (current-ring))
    '()  
    (ring-neighbors (current-ring) n)))

(define (landing-node? n)
  (and (not (empty? (current-parent-stack)))
       (not (empty? (ring-nodes (current-ring))))
       (eq? n (first (ring-nodes (current-ring))))))

(define (current-level-parent n)
  (cond 
    ;No stack.  You are a top parent
    [(empty? (current-parent-stack)) #f]

    ;Last one on the stack?  You are at the top level and have no parents.
    [(eq? n (last (current-parent-stack))) 
     #f]

    ;Not on the stack? Then the first node on the stack is your parent
    [(not (member n (current-parent-stack))) 
     (first (current-parent-stack))]

    ;In the stack?  The next node up deeper in the stack is your parent.
    [else (list-ref (current-parent-stack) 
                    (add1 (index-of (current-parent-stack) n)))]))

(define (current-depth-of n)
  (cond
    [(empty? (current-parent-stack)) 0]
    [(not (member n (current-parent-stack))) (length (current-parent-stack))]
    [else (index-of (current-parent-stack) n)]))

(define (node-scale n (depth (current-depth-of n)))
  (if (= 0 depth)
    1
    (/ 1 (expt (level-depth) 
               depth))))

;Offset the nodes in a ring by their level parent's
;  position.  So that (0 0) is directly beneath the parent.
(define (calc-absolute node-* 
                       n 
                       (depth (current-depth-of n)))

  (define my-* (* (node-scale n depth)
                  (node-* n)))

  (define parent (current-level-parent n))

  (+ my-*
     (if (not parent)
       0
       (calc-absolute node-* parent (sub1 depth)) )))


(define (first-in-current-ring n)
  (eq? (get-id n)
       (get-id (first (ring-nodes (current-ring))))))

(define (node->step n)
  (cond
    [(ring? n)  (ring->steps n)]
    [(level? n) (level->steps n)]
    [else 
      (step #:x (~r (calc-absolute node-x n)
                    #:precision 3)   
            #:y (~r (calc-absolute node-y n)
                    #:precision 3)    
            #:z (* -1000 (length (current-parent-stack)))
            #:scale (~r (node-scale n)
                        #:precision 3)
            id: (node-id n)
            'onClick: (~a "if(window.location.href.includes('#/" (node-id n) "')){setTimeout(() => impress().goto('" 
                            (cond 
                              [(current-child) (get-id (current-child))]
                              [(and (current-level-parent n)
                                    (first-in-current-ring n)) (get-id (current-level-parent n))]
                              [else ""]) 

                            "'), 1)}")
            (node-content n)
            (when (impress-debug)
              (list
                @div{
                id:@(get-id n), 
                pid:@(get-id (current-level-parent n)),
                cid:@(get-id (current-child)),
                d:@(current-depth-of n)
                }
                (hr)
                @div{relative, x:@(node-x n),y:@(node-y n)}
                (hr)))
            )]))

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
