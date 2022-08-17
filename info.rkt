#lang info
(define collection "stencil-vector-utils")
(define deps '("base" ("racket" #:version "8.6")))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/stencil-vector-utils.scrbl" ())))
(define pkg-desc "Functions to make working with stencil vectors easier")
(define version "0.6.1")
(define pkg-authors '(shawnw))
(define license '(Apache-2.0 OR MIT))
