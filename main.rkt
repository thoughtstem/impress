#lang at-exp racket

(require (except-in website/impress site time) 
         (except-in website/bootstrap col site time)  
         (except-in 2htdp/image frame)
         "./util.rkt")

(require posn)


;Rings are like sibling you can navigate between (currently with right and left, later perhaps with cardinal directions -- like an net)

;From anywhere in a ring, you can back up a level to the ring's parent

;Nodes in rings may be parents of other rings, and so on.  Hopefully allowing for infinite organizational freedom.  At each zoom in level, you have more space...


;TODO: y direction is not handled yet in all places (offset-y)


(struct node (id x y content) #:transparent)
(struct ring (nodes) #:transparent)
(define ring-nav-keys  '("ArrowLeft" "ArrowRight"))
(define level-nav-keys '("ArrowUp" "ArrowDown"))

(struct level (node ring))

(define (make-node #:id (id (~a (gensym 'id))) x y content)
  (node id x y content))

(define (make-ring . ns)
  (ring ns))

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
(define depth 3)

(define (node-scale n)
  (exact->inexact 
    (expt depth (- (length (current-parent-stack))))))

(define (current-ring-keys n)
  (if (current-ring)
    ring-nav-keys
    '()))

(define (current-ring-neighbors n)
  (if (not (current-ring))
    '()  
    (ring-neighbors (current-ring) n)))

(define (current-level-keys n)
  (filter-not void?
              (list
                (when (current-child)
                  "ArrowUp") 
                (when (not (empty? (current-parent-stack)))
                  "ArrowDown"))))

(define (current-level-neighbors n)
  (filter-not void?
              (list
                (when (current-child)
                  (current-child))
                (when (not (empty? (current-parent-stack)))
                  (first (current-parent-stack))))))

(define (node-nav-keys n)
  (string-join
    (flatten
      (append
        (current-ring-keys n)
        (current-level-keys n)))
    " "))

(define (node-next-list n)
  (string-join
    (map node-id
         (filter identity
                 (flatten
                   (append
                     (current-ring-neighbors n)
                     (current-level-neighbors n)))))
    " "))

(define (offset-x n)
  (define (adj x i)
    (* x (expt depth (- i))))

  (if (empty? (current-parent-stack))
    0
    (apply + 
           (map adj 
                (map node-x (current-parent-stack))
                (reverse (range (length (current-parent-stack)))) ))))

(define (offset-y n)
  (if (empty? (current-parent-stack))
    0
    (apply + (map node-y (current-parent-stack)))))


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
                #:goto (node-id n)
                #:key-list  (node-nav-keys n)
                #:next-list (node-next-list n) 
                id: (node-id n)
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
      (node->step n))
    
    )

  (define r-steps
    (parameterize ([current-parent-stack (cons n (current-parent-stack))]) 
      (ring->steps r)))

  (cons n-step r-steps))

(begin
  (define a (make-node #:id "a"
                       0 0 
                       (write-img 
                         (circle 20 'outline 'red))))

  (define b (make-node #:id "b" 
                       200 0 
                       (write-img 
                         (circle 20 'outline 'blue))))


  (define ax (make-node #:id "ax"
                        0 0 
                        (write-img 
                          (circle 20 'outline 'red))))

  (define ay (make-node #:id "ay" 
                        100 0 
                        (write-img 
                          (circle 20 'outline 'red))))

  (define ay-1 (make-node #:id "ay-1"
                        0 0 
                        (write-img 
                          (star 20 'solid 'red))))

  (define bx (make-node #:id "bx"
                        0 0 
                        (write-img 
                          (circle 20 'outline 'blue))))

  (define by (make-node #:id "by"
                        100 0 
                        (write-img 
                          (circle 20 'outline 'blue))))


  (define by-1 (make-node #:id "by-1"
                        0 0 
                        (write-img 
                          (star 20 'solid 'blue))))

  (define ra (make-ring ax ;Putting a level as the first thing in the ring causes an error
                        (level ay
                               (make-ring ay-1))))
  (define rb (make-ring bx (level by 
                                  (make-ring by-1))))

  (define la (level a ra))
  (define lb (level b rb))

  (define r-top (make-ring la lb))

  (define steps
    (ring->steps r-top)))


(define (test)
  (impress-me steps )) 

(render (test) #:to "out") 
