#lang racket/base

(require racket/file
         racket/match
         racket/string)

(provide load-options)

(define (comment-or-whitespace? line)
  (regexp-match? #px"^\\s*(?:#.*|;.*|)$" line))

(define (line->keyval line)
  (match (regexp-match #px"^\\s*([^ :]+)\\s*:\\s*(.+)$" line)
    [(list _ keystr val)
     (list (string->symbol keystr)
           (string-trim val #:left? #f))]
    [_ #f]))

(define (load-options filename #:defaults [opts-hash (hasheq)])
  (for/hash ([line (in-list (file->lines filename))]
             #:when (not (comment-or-whitespace? line))
             #:do [(define opt-pair (line->keyval line))]
             #:when opt-pair)
    (apply values opt-pair)))

(module+ test
  (require rackunit)

  (check-false (comment-or-whitespace? "abc"))
  (check-false (comment-or-whitespace? "  abc"))
  (check-false (comment-or-whitespace? "  abc"))
  (check-false (comment-or-whitespace? "a #bc"))
  (check-false (comment-or-whitespace? "a ;bc"))
  
  (check-true (comment-or-whitespace? ""))
  (check-true (comment-or-whitespace? " "))
  (check-true (comment-or-whitespace? "\t "))
  (check-true (comment-or-whitespace? "#"))
  (check-true (comment-or-whitespace? ";"))
  (check-true (comment-or-whitespace? "# a"))
  (check-true (comment-or-whitespace? "; a  "))
  (check-true (comment-or-whitespace? " ; a  "))
  (check-true (comment-or-whitespace? " # a  "))

  (check-false (line->keyval "abc"))
  (check-equal? (line->keyval "a:b") '(a "b"))
  (check-equal? (line->keyval " a : b ") '(a "b"))
  (check-equal? (line->keyval " a ::b") '(a ":b"))
  (check-equal? (line->keyval "a: b:c  ") '(a "b:c")))