#lang info
(define collection "canned")
(define deps '("gui-lib"
               "net-lib"
               "base"))
(define build-deps '("at-exp-lib"
                     "scribble-lib" "racket-doc" "rackunit-lib"))
(define raco-commands '(("canned" (submod canned/command raco) "issue canned response" #f)))
(define pkg-desc "Generate canned responses that would otherwise be fussy to write.")
(define version "0.2")
(define pkg-authors '(joeld))
