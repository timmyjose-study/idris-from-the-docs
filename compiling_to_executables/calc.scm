(define (my-add x y)
  (+ x y))

(define (my-sub x y)
  (- x y))

(define (my-mul x y)
  (* x y))

(define (my-div x y)
  (if (zero? y)
      0
      (/ x y)))
