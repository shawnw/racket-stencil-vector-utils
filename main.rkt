#lang racket/base

;;; Extra functions for Racket 8.6 stencil vectors

(require racket/require racket/contract (only-in racket/function thunk))
(require (for-syntax racket/base (only-in racket/string string-prefix?)))
(require (filtered-in
          (lambda (name)
            (and (string-prefix? name "unsafe-fx")
                 (substring name 7)))
          racket/unsafe/ops))
(module+ test
  (require rackunit))

(provide (contract-out
          [stencil-vector-slot? contract?]
          [stencil-vector-bitmask? contract?]
          [stencil-vector-slot->index (-> stencil-vector? stencil-vector-slot? (or/c exact-nonnegative-integer? #f))]
          [stencil-vector-index->slot (-> stencil-vector? exact-nonnegative-integer? stencil-vector-slot?)]
          [stencil-vector-slot-ref (->* (stencil-vector? stencil-vector-slot?) (any/c) any)]
          [stencil-vector-has-slot? (-> stencil-vector? stencil-vector-slot? boolean?)]
          [in-stencil-vector (-> stencil-vector? sequence?)]
          [stencil-vector->list (-> stencil-vector? list?)]
          [stencil-vector->vector (-> stencil-vector? vector?)]
          [stencil-vector-for-each (-> (-> any/c any/c) stencil-vector? void?)]
          [stencil-vector-map! (-> (-> any/c any/c) stencil-vector? void?)]
          [stencil-vector-map (->* ((-> any/c any/c) stencil-vector?) (#:bitmask stencil-vector-bitmask?) stencil-vector?)]
          [stencil-vector-fold (-> (-> any/c any/c any/c) any/c stencil-vector? any/c)]
          ))

(define stencil-vector-slot? (integer-in 0 (sub1 (stencil-vector-mask-width))))
(define stencil-vector-bitmask? (integer-in 0 (sub1 (expt 2 (stencil-vector-mask-width)))))

(define (stencil-vector-slot->index sv slot)
  (let ([mask (stencil-vector-mask sv)]
        [pos (fxlshift 1 slot)])
    (if (fx> (fxand mask pos) 0)
        (fxpopcount (fxand mask (- pos 1)))
        #f)))

(define (stencil-vector-index->slot sv n)
  (unless (< n (stencil-vector-length sv))
    (raise-argument-error 'stencil-vector-index->slot "Index out of range" n))
  (let loop ([slot 0]
             [n n])
    (if (fx> (fxand (fxlshift 1 slot) (stencil-vector-mask sv)) 0)
        (if (= n 0)
            slot
            (loop (+ slot 1) (- n 1)))
        (loop (+ slot 1) n))))

(define (stencil-vector-slot-ref sv i [default (lambda () (error "Index not present: " i))])
  (let ([n (stencil-vector-slot->index sv i)])
    (if n
        (stencil-vector-ref sv n)
        (if (procedure? default)
            (default)
            default))))

(define (stencil-vector-has-slot? sv i)
  (> (fxand (stencil-vector-mask sv) (fxlshift 1 i)) 0))

(define (in-stencil-vector sv)
  (make-do-sequence
   (thunk
    (values
     (lambda (i) (stencil-vector-ref sv i))
     add1
     0
     (lambda (i) (< i (stencil-vector-length sv)))
     #f
     #f))))

(define (stencil-vector->list sv)
  (for/list ([elem (in-stencil-vector sv)]) elem))

(define (stencil-vector->vector sv)
  (for/vector #:length (stencil-vector-length sv)
              ([elem (in-stencil-vector sv)])
    elem))

(define (stencil-vector-for-each proc sv)
  (for ([elem (in-stencil-vector sv)])
    (proc elem)))

(define (stencil-vector-map! proc sv)
  (for ([n (in-range (stencil-vector-length sv))])
    (stencil-vector-set! sv n (proc (stencil-vector-ref sv n)))))

(define (stencil-vector-map proc sv #:bitmask [bitmask (stencil-vector-mask sv)])
  (unless (= (stencil-vector-length sv) (fxpopcount bitmask))
    (raise-argument-error 'stencil-vector-map "bitmask has different arity than stencil vector argument" bitmask))
  (apply stencil-vector bitmask (for/list ([elem (in-stencil-vector sv)]) (proc elem))))

(define (stencil-vector-fold kons knil sv)
  (for/fold ([acc knil])
            ([n (in-range (stencil-vector-length sv))])
    (kons (stencil-vector-ref sv n) acc)))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (define sv1 (stencil-vector #b10101 'a 'b 'c))

  (check-true (stencil-vector-has-slot? sv1 0))
  (check-false (stencil-vector-has-slot? sv1 1))
  (check-true (stencil-vector-has-slot? sv1 2))
  (check-false (stencil-vector-has-slot? sv1 3))
  (check-true (stencil-vector-has-slot? sv1 4))

  (check-equal? (stencil-vector-index->slot sv1 0) 0)
  (check-equal? (stencil-vector-index->slot sv1 1) 2)
  (check-equal? (stencil-vector-index->slot sv1 2) 4)

  (check-equal? (stencil-vector-slot->index sv1 0) 0)
  (check-equal? (stencil-vector-slot->index sv1 1) #f)
  (check-equal? (stencil-vector-slot->index sv1 2) 1)
  (check-equal? (stencil-vector-slot->index sv1 3) #f)
  (check-equal? (stencil-vector-slot->index sv1 4) 2)
  (check-equal? (stencil-vector-slot->index sv1 5) #f)

  (check-equal? (stencil-vector-slot-ref sv1 0) 'a)
  (check-equal? (stencil-vector-slot-ref sv1 1 #f) #f)

  (check-equal? (stencil-vector->list sv1) '(a b c))
  (check-equal? (stencil-vector->vector sv1) '#(a b c))

  (stencil-vector-map! symbol->string sv1)
  (check-equal? (stencil-vector->list sv1) '("a" "b" "c"))
  (define sv2 (stencil-vector-map #:bitmask #b11010 string->symbol sv1))
  (check-equal? (stencil-vector->list sv2) '(a b c))
  (check-false (stencil-vector-has-slot? sv2 0))
  (check-true (stencil-vector-has-slot? sv2 1))
  (check-false (stencil-vector-has-slot? sv2 2))
  (check-true (stencil-vector-has-slot? sv2 3))
  (check-true (stencil-vector-has-slot? sv2 4))

  (check-equal? (stencil-vector-fold (lambda (s total) (+ (string-length s) total)) 0 sv1) 3)

  )
