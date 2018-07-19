#lang racket
(require "suite.rkt")

(module+ main
  (define path/scheme->odefa "../scheme-\\>odefa.rkt")
  (define path/odefa/cases "cases/odefa")
  (define path/scheme/cases "cases/scheme")

  (void (system (~a "raco make " path/scheme->odefa)))

  (define sources
    (for/list ([test suite])
      (match-define (Test _ test-cases) test)
      (match-define (TestCase _ source) (dict-ref test-cases 'ddpa))
      source))

  (for ([source (remove-duplicates sources)])
    (define command
      (~a "racket " path/scheme->odefa " "
          "< " path/scheme/cases "/" source ".scm "
          "> " path/odefa/cases "/" source ".odefa"))
    (display (~a command "..."))
    (flush-output)
    (system command)
    (displayln " done")
    (flush-output)))