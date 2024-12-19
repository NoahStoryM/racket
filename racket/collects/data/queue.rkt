#lang racket/base
(require racket/contract/base
         racket/mutability
         (for-syntax racket/base
                     syntax/contract))

;; A Queue contains a linked list with mutable cdrs, holding two pointers
;; to the head and the tail -- where items are pulled from the head and
;; pushed on the tail.  It is not thread safe: mutating a queue from
;; different threads can break it.
(struct queue (head tail length) #:mutable
  #:property prop:sequence (λ (q) (in-queue q)))
;; (Note: uses #f for `head' to mark an empty queue, but in those cases
;; the tail will be set to #f too, to avoid holding on to values that
;; should be collected.)
(struct link (value [tail #:mutable]))

(define (make-queue . v*) (list->queue v*))

(define (queue-empty? q) (not (queue-head q)))

(define (non-empty-queue? v) (and (queue? v) (queue-head v) #t))

(define (enqueue! q v)
  (let ([new (link v #f)])
    (if (queue-head q)
      (set-link-tail! (queue-tail q) new)
      (set-queue-head! q new))
    (set-queue-tail! q new)
    (set-queue-length! q (+ (queue-length q) 1))))

(define (enqueue-front! q v)
  (define fr (queue-head q))
  (cond
    [fr
     (set-queue-head! q (link v fr))]
    [else
     (define k (link v #f))
     (set-queue-head! q k)
     (set-queue-tail! q k)])
  (set-queue-length! q (+ (queue-length q) 1)))

(define (dequeue! q)
  (let ([old (queue-head q)])
    (cond
      [(eq? old (queue-tail q))
       (set-queue-tail! q #f)
       (set-queue-head! q #f)]
      [else
       (set-queue-head! q (link-tail old))])
    (set-queue-length! q (- (queue-length q) 1))
    (link-value old)))

(define (list->queue v*)
  (define len (length v*))
  (if (= len 0)
      (queue #f #f 0)
      (let* ([head (link (car v*) #f)]
             [q (queue head head len)])
        (for ([v (in-list (cdr v*))])
          (define new (link v #f))
          (set-link-tail! (queue-tail q) new)
          (set-queue-tail! q new))
        q)))

(define (vector->queue v*)
  (define len (vector-length v*))
  (if (= len 0)
      (queue #f #f 0)
      (let* ([head (link (vector-ref v* 0) #f)]
             [q (queue head head len)])
        (for ([v (in-vector 1 v*)])
          (define new (link v #f))
          (set-link-tail! (queue-tail q) new)
          (set-queue-tail! q new))
        q)))

(define (queue->list q) (for/list ([e (in-queue q)]) e))

(define (queue->vector q) (for/vector #:length (queue-length q) ([e (in-queue q)]) e))

(define (queue-filter! q pred?)
  (unless (queue-empty? q)
    (let loop ([prev #f]
               [curr (queue-head q)]
               [i 0])
      (cond
        [(not curr)
         (set-queue-tail! q prev)
         (set-queue-length! q i)]
        [else
         (define passed? (pred? (link-value curr)))
         (cond
           [passed?
            (loop curr (link-tail curr) (+ i 1))]
           [else
            (define tl (link-tail curr))
            (if prev
                (set-link-tail! prev tl)
                (set-queue-head! q tl))
            (loop prev tl i)])]))))

(define (in-queue q)
  (make-do-sequence
   (λ ()
     (values
      link-value
      link-tail
      (queue-head q)
      link?
      #f #f))))

(define-sequence-syntax in-queue*
  (lambda () #'in-queue)
  (lambda (stx)
    (syntax-case stx ()
      ([(var) (in-queue* queue-expression)]
       (with-syntax ([queue-expression/c (wrap-expr/c #'queue? #'queue-expression
                                                      #:macro #'in-queue*)])
         #'[(var)
            (:do-in ([(queue) queue-expression/c])
                    (void) ;; handled by contract
                    ([link (queue-head queue)])
                    link
                    ([(var) (link-value link)])
                    #t
                    #t
                    ((link-tail link)))]))
      ([(var ...) (in-queue* queue-expression)]
       #f))))

;; --- contracts ---
(define queue/c queue?)
(define nonempty-queue/c non-empty-queue?)

(provide
 (contract-out
  [queue/c flat-contract?]
  [nonempty-queue/c flat-contract?]
  [queue? (-> any/c boolean?)]
  [non-empty-queue? (-> any/c boolean?)]
  [make-queue (-> any/c ... queue?)]
  [queue-empty? (-> queue? boolean?)]
  [queue-length (-> queue? exact-nonnegative-integer?)]
  [list->queue (-> list? queue?)]
  [vector->queue (-> vector? queue?)]
  [queue->list (-> queue? list?)]
  [queue->vector (-> queue? mutable-vector?)]
  [queue-filter! (-> queue? (-> any/c any/c) void?)]
  [enqueue! (-> queue? any/c void)]
  [enqueue-front! (-> queue? any/c void)]
  [dequeue! (-> non-empty-queue? any/c)])
 (rename-out [in-queue* in-queue]))
