(library (bee statistic (1 0 0))
  (export bee-mileage)
  (import (chezscheme))

  ;;; @brief time the <trunk>
  ;;; @param trunk (lambda () ...)
  ;;; @return (values mileage result)
  ;;; @refer make-engine
  ;;; @test
  ;;; (bee-mileage (lambda () (display "statistic: \n") 'hello))
  ;;; => statistic:
  ;;;    hello
  ;;;    34
  (define bee-mileage
    (lambda (thunk)
      (let loop ([eng (make-engine thunk)] [total-ticks 0] [fuel 10000])
        (eng fuel
             (lambda (ticks value)
               (values value (+ total-ticks (- fuel ticks))))
             (lambda (new-eng)
               (loop new-eng (+ total-ticks fuel) fuel))))))
  )