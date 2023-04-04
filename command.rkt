#lang racket/base

(require "main.rkt"
         racket/cmdline
         racket/logging
         racket/vector)

(define (raco-canned-spf)
  (define-values (domain sender-ip)
    (command-line #:program "raco canned spf"
                  #:argv (vector-drop (current-command-line-arguments) 1)
                  #:args (domain sender-ip)
                  (values domain sender-ip)))
  (fill-report domain sender-ip))

(define (raco-canned-help)
  (displayln "Canned Response commands:
help                     Show this message
spf domain sender-ip     Generate notice of broken SPF"))

(define (dispatch subcommand-name)
  (define subcommand-proc
    (case subcommand-name
      [("spf") raco-canned-spf]
      [else raco-canned-help]))
  
  (with-logging-to-port (current-error-port)
    subcommand-proc
    #:logger canned-logger
    'info))

(module+ raco
  (define subcommand
    (with-handlers ([exn:fail? (λ (exn) #f)])
      (vector-ref (current-command-line-arguments) 0)))
  (dispatch subcommand))