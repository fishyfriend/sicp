;; Exercise 1.22 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (begin (report-prime (- (runtime) start-time)) #t)
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; Set upper to -1 to remove the upper bound
(define (search-for-primes lower upper max-results)
  (define (go n bound results)
    (if (and (or (= bound -1)
                 (not (> n bound)))
             (< results max-results))
        (go (+ n 2)
            bound
            (+ results
               (if (timed-prime-test n)
                   1
                   0)))))
  (go (if (even? lower)
          (+ lower 1)
          lower)
      upper
      0))

(define (start-prime-test n start-time)
  (define (repeat times)
          (if (> times 0)
              (begin (prime? n)
                     (repeat (- times 1)))))
  (if (prime? n)
      (begin (repeat prime-test-n-reps)
             (report-prime (- (runtime) start-time))
             #t)
      #f))

(define prime-test-n-reps 9999)

(search-for-primes (expt 10 3) -1 3)
; primes: 1009, 1013, 1019; timings: 0.29s, 0.28s, 0.29s

(search-for-primes (expt 10 4) -1 3)
; primes: 10007, 10009, 100037; timings: 0.92s, 0.93s, 0.93s

(search-for-primes (expt 10 5) -1 3)
; primes: 100003, 100019, 100043; timings: 2.91s, 3.10s, 2.93s

(search-for-primes (expt 10 6) -1 3)
; primes: 1000003, 1000033, 1000037; timings: 9.26s, 9.26s, 9.19s

#| The timings for primes up to 10^6 reflect an order of growth of Θ(√n). For
each tenfold increase in n, the runtime grows approximately √10 times. This
behavior is consistent with an order of growth Θ(√n).

This conclusion was reached after modifying the test code to account for the
high speed of this particular test system. More specifically, the procedure
start-prime-test has been replaced with a version that repeats its primality
test an additional 9999 times.

The modification was necessary because without it, the runtimes for n in this
range were too short to be useful for reasoning about the order of growth. The
runtime procedure reports in 0.01s increments, and the reported runtimes for
primes up to 10^6 were all essentially zero. Thus, even if our prime test were
exhibiting Θ(√n) growth, the growth in reported timings would be flat (implying,
incorrectly, Θ(1)). |#

;; Exercise 1.23 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (next x)
  (if (= x 2) 3 (+ x 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(search-for-primes (expt 10 3) -1 3) ; 0.20s, 0.20s, 0.21s
(search-for-primes (expt 10 4) -1 3) ; 0.60s, 0.60s, 0.60s
(search-for-primes (expt 10 5) -1 3) ; 1.83s, 1.83s, 1.81s
(search-for-primes (expt 10 6) -1 3) ; 5.84s, 5.84s, 5.85s

#| The new algorithm runs takes a bit more than 60% as long as the old
algorithm, worse than the expected 50%. The discrepancy can be explained by the
observation that, while the new approach eliminates half of the test steps, it
also adds new computations within each step. Namely, it replaces a single
addition (+ test-divisor 1) with a procedure call (next test-divisor) whose
evaluation will include an addition, an equality check and conditional branch.
Roughly speaking, we've replaced n steps of size z with (n/2) steps of size
(6z/5):

  original runtime = nz
  revised runtime = (n/2)(6z/5) = 3nz/5 = (60/100)nz = 60% * original runtime |#

;; Exercise 1.24 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (prime? n)
  (fast-prime? n 10))

(search-for-primes (expt 10 3) -1 3) ; 1.79s, 1.87s, 1.93s
(search-for-primes (expt 10 4) -1 3) ; 2.29s, 2.24s, 2.31s
(search-for-primes (expt 10 5) -1 3) ; 2.59s, 2.67s, 2.70s
(search-for-primes (expt 10 6) -1 3) ; 3.03s, 3.01s, 3.18s

#| We would expect the Fermat test for primes near 1,000,000 to take double the
time needed for primes near 1000 since log 1000000 = log 1000² = 2 log 1000.
This expectation does not quite hold up empirically on my machine: the increase
in running time from n=1000 to n=1000000 is closer to 1.65x rather than the
expected 2x. To understand the discrepancy, note that the calculation includes
some constant-cost, non-logarithmic elements. Those elements must account for a
non-trivial fraction of the processing time for n <= 1000000.

Since we do not know what fraction of the reported timings is constant-cost and
what fraction is logarithmic, it will be difficult to verify the order of growth
empirically by simply comparing two absolute runtimes. We must instead look at
the runtime deltas, which by virtue of being deltas will not be affected by
constant costs. Considering the reported runtimes for n=10^x, moving stepwise
from x=3 to x=6, we find that the increase in runtime for each step is fairly
consistent. Runtime increases about 0.4s for each tenfold increase in n. This
observation confirms that the order of growth for the procedure is in fact
logarithmic. |#

;; Exercise 1.25 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fast-expt b n)
i  (cond ((= n 0) 1)
         ((even? n) (square (fast-expt b (/ n 2))))
         (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

#| The revised expmod procedure is logically equivalent to the original. Its
order of growth also appears equivalent to the original in terms of both running
time and memory usage. Observe that both versions generate a deferred
computation where the number of steps grows as log(exp).

The above analysis assumes that time and space requirements are the same for all
operations on numeric values of any size. If that assumption doesn't hold, the
orders of growth could be different. In the original version we compute the
remainder as a single step at the end. In the revised version we interleave
remainder calls with square and *. The latter approach ensures that a quantity
larger than m is never passed as the first argument to remainder or square, or
as the second argument to *. If the order of growth of remainder, *, or any
numeric primitives used inside square is worse than Θ(1) in either time or
space, then the original expmod could be less efficient since the computation
it generates deals with larger numbers (up to base^exp) as arguments. |#

;; Exercise 1.26 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

#| Like the original expmod, this procedure builds up a deferred computation
where the number of steps grows as the total number of recursive calls to
itself. If we can determine how the number of recursive calls grows in relation
to exp, then we will also have determined the order of growth of the procedure
in terms of both time and space.

Imagine we tag each execution of expmod with a depth attribute. The outermost
expmod has depth 0, any recursive calls it generates have depth 1, and so on.
Let i be the maximum depth reached during a particular invocation of expmod. We
can see that i grows as log₂(exp) since we divide exp by two repeatedly until
reaching the stopping condition. We can also see that the total number of
recursive calls grows as 2ⁱ since we make two recursive calls each time we
encounter the (even? exp) branch. So the order of growth is Θ(2ⁱ), which reduces
to linear:

  Θ(2ⁱ) = Θ(2^(log₂ exp)) = Θ(exp) |#

;; Exercise 1.27 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fools-fermat-test? n)
  (define (go a n)
    (cond ((= a 0) #t)
          ((= (expmod a n n) a) (go (- a 1) n))
          (else #f)))
  (go (- n 1) n))

(fools-fermat-test? 561) ;#t
(fools-fermat-test? 1105) ;#t
(fools-fermat-test? 1729) ;#t
(fools-fermat-test? 2465) ;#t
(fools-fermat-test? 2821) ;#t
(fools-fermat-test? 6601) ;#t

;; Exercise 1.28 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expmod base exp m)
  (define (next x)
    (if (nontrivial-sqrt? x m)
        0
        (remainder (square x) m)))
  (cond ((= exp 0) 1)
        ((even? exp)
         (next (expmod base (/ exp 2) m)))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

(define (nontrivial-sqrt? x n)
  (and (not (= x 1))
       (not (= x (- n 1)))
       (= (remainder (square x) n) 1)))

(define (miller-rabin-test n)
  (define (try-it a)
    (not (= (expmod a (- n 1) n) 0)))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n 100))

#| With the Fermat test it was sufficient to repeat the test 10 times, but here
I repeat 100 times as there were occasional errors using only 10. The text
misleadingly claims that the Miller-Rabin test is foolproof, but it is a
probabilistic algorithm just like the Fermat test. It would be more correct to
claim only that there are no numbers which consistently fool the Miller-Rabin
test. That is still an improvement over the Fermat test, which is always fooled
by Carmichael numbers. |#

; known prime numbers
(prime? 1009) ;#t
(prime? 10007) ;#t
(prime? 100003) ;#t
(prime? 1000003) ;#t

; known non-prime numbers
(prime? 1000) ;#f
(prime? 10000) ;#f
(prime? 100000) ;#f
(prime? 1000000) ;#f

; Carmichael numbers
(prime? 561) ;#f
(prime? 1105) ;#f
(prime? 1729) ;#f
(prime? 2465) ;#f
(prime? 2821) ;#f
(prime? 6601) ;#f

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; Exercise 1.29 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simpsons-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k) (+ (* 4 (y k)) (* 2 (y (+ k 1)))))
  (define (next k) (+ k 2))
  (* (/ h 3)
     (+ (y 0)
        (sum term 1 next (- n 1))
        (y n))))

(integral cube 0 1 0.01)          ;          .24998750000000042
(integral cube 0 1 0.001)         ;          .249999875000001
(simpsons-integral cube 0 1 100)  ; 77/300 ≈ .25666666666666665
(simpsons-integral cube 0 1 1000) ; 94/375 ≈ .25066666666666665

;; Exercise 1.30 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; Exercise 1.31 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; a.

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (define (term k) k)
  (product term 1 inc n))

(define (approx-pi n)
  (define (frac k) (/ (+ k 1) (+ k 2)))
  (define (term k) (if (odd? k) (frac k) (/ 1 (frac k))))
  (* 4 (product term 1 inc n)))

(approx-pi 10)   ; ≈ 3.2751010413348074
(approx-pi 100)  ; ≈ 3.1570301764551676
(approx-pi 1000) ; ≈ 3.1431607055322663

; b.
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;; Exercise 1.32 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; a.

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

; b.

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;; Exercise 1.33 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (filtered-accumulate combiner null-value term a next b pred)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (pred a)
                  (combiner result (term a))
                  result))))
  (iter a null-value))

; a.

(define (sum-of-squares-of-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

; b.

(define (product-of-relative-primes n)
  (define (term i) i)
  (define (pred i) (= (gcd i n) 1))
  (filtered-accumulate * 1 term 1 inc (- n 1) pred))

;; Exercise 1.34 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (f g)
  (g 2))

#| Evaluating (f f) in the interpreter results in an error: "The object 2 is not
applicable." During evaluation the interpreter first substitutes f's argument
into the body of the procedure, yielding (f 2). Now to evaluate (f 2) it must
perform same substitution, yielding (2 2). Here the trouble occurs. When
evaluating a combination we first evaluate the subexpressions, and we expect the
left most subexpression (the operator) to evaluate to a procedure. In this case,
though, it is the literal 2, which is not a procedure. Evaluating a combination
whose operator is not a procedure does not make sense, so the interpreter cannot
proceed. |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average a b) (/ (+ a b) 2))

;; Exercise 1.35 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| Given

  ϕ = (1 + √5)/2 and
  f(x) = 1 + 1/x,

we can show that f(ϕ) = ϕ as follows:

  f(ϕ) = 1 + 1/ϕ
       = 1 + 1/((1 + √5)/2)
       = 1 + 2/(1 + √5)
       = (1 + √5 + 2)/(1 + √5)
       = (3 + √5)/(1 + √5)
       = (3 + √5)(1 - √5)/(1 + √5)(1 - √5)
       = (3 - 2√5 - 5)/(1 - 5)
       = (-2 - 2√5)/(-4)
       = (1 + √5)/2
       = ϕ |#

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) ; 1.6180327868852458

;; Exercise 1.36 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| The below version of fixed-point displays the current approximation prefixed
by a step count. It takes 34 steps to reach an acceptable answer without average
damping. With average damping, the number of steps is reduced to 9. The
transformation of the equation to introduce average damping is shown.

  x^x = 1000
  x   = log(1000)/log(x)
  2x  = log(1000)/log(x) + x
  x   = (log(1000)/log(x) + x)/2 |#

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess step)
    (let ((next (f guess)))
      (display ";") (display step) (display " : ") (display next) (newline)
      (if (close-enough? guess next)
          next
          (try next (+ 1 step)))))
  (try first-guess 1))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

  ;1 : 9.965784284662087
  ;2 : 3.004472209841214
  ; ...
  ;33 : 4.555540912917957
  ;34 : 4.555532270803653
  ;Value: 4.555532270803653

(fixed-point (lambda (x) (/ (+ (/ (log 1000) (log x)) x) 2)) 2.0)

  ;1 : 5.9828921423310435
  ;2 : 4.922168721308343
  ; ...
  ;8 : 4.5555465521473675
  ;9 : 4.555537551999825
  ;Value: 4.555537551999825

;; Exercise 1.37 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; a.

(define (cont-frac n d k)
  (define (iter i)
    (/ (n i) (denom i)))
  (define (denom i)
    (if (= i k)
        (d i)
        (+ (d i)
           (iter (+ i 1)))))
  (iter 1))

#| test-cont-frac approximates 1/ϕ for successive values of k, printing out each
value of k and the corresponding result, stopping when the result is accurate to
4 decimal places. The stopping condition is achieved at k=10. |#

(define (test-cont-frac)
  (define (accurate-enough? guess)
    (= (round-to-decimal-places guess 4)
       (round-to-decimal-places (/ 1 phi) 4)))
  (define (iter k)
    (let ((guess (approximate-one-over-phi k)))
      (display ";") (display k) (display " : ") (display guess) (newline)
      (if (accurate-enough? guess)
          guess
          (iter (+ k 1)))))
  (iter 1))

(define phi (/ (+ 1 (sqrt 5)) 2))

(define (approximate-one-over-phi k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

(define (round-to-decimal-places x n)
  (define scale (expt 10 n))
  (/ (round (* scale x)) scale))

(test-cont-frac)

  ;1 : 1.
  ;2 : .5
  ;3 : .6666666666666666
  ;4 : .6000000000000001
  ;5 : .625
  ;6 : .6153846153846154
  ;7 : .6190476190476191
  ;8 : .6176470588235294
  ;9 : .6181818181818182
  ;10 : .6179775280898876
  ;Value: .6179775280898876

; b.

(define (cont-frac n d k)
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (- i 1)
              (/ (n i)
                 (+ (d i) acc)))))
  (iter (- k 1) (/ (n k) (d k))))

;; Exercise 1.38 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (approximate-e k)
  (define (n i) 1.0)
  (define (d i)
    (if (= (remainder i 3) 2)
        (* 2 (+ (quotient i 3) 1))
        1))
  (+ (cont-frac n d k) 2))

(approximate-e 100) ; 2.7182817182817183

;; Exercise 1.39 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tan-cf x k)
  (/ (cont-frac (lambda (i) (- (square x)))
                (lambda (i) (- (* 2 i) 1))
                k)
     (- x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;; Exercise 1.40 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cube x) (* x x x))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

; (x+3)(x+2)(x+1) = x³ + 6x² + 11x + 6 = (cubic 6 11 6) with zeros -1, -2, -3
(newtons-method (cubic 6 11 6) 1) ; -.9999999999359223 ≈ -1

;; Exercise 1.41 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

(((double (double double)) inc) 5) ; 21

;; Exercise 1.42 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6) ; 49

;; Exercise 1.43 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5) ; 625

;; Exercise 1.44 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (smooth f dx)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define (n-fold-smooth f n dx)
  ((repeated (lambda (f) (smooth f dx)) n) f))

;; Exercise 1.45 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| (f x n) returns a procedure for y ↦ x/yⁿ⁻¹, the function whose fixed points
are solutions to g(x) = ⁿ√x. |#

(define (f x n)
  (lambda (y) (/ x (expt y (- n 1)))))

#| (fixed-point-steps f first-guess max-steps) returns the number of steps that
(fixed-point f first-guess) would require to converge on a solution, or #f if it
would not converge within max-steps steps. |#

(define (fixed-point-count-steps f first-guess max-steps)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess step)
    (let ((next (f guess)))
      (cond ((>= step max-steps) #f)
            ((close-enough? guess next) step)
            (else (try next (+ 1 step))))))
  (try first-guess 1))

#| nth-root-test counts and displays the number of steps required for
convergence of fixed-point search on f for each combination of average damps d,
x, and n within the specified ranges, using the *inc procedures to increment
those quantities. When a non-converging test case is encountered, the rest of
the cases for the current value of d are skipped. |#

(define (nth-root-test dmin dmax dinc
                       xmin xmax xinc
                       nmin nmax ninc
                       first-guess max-steps)
  (define (iter d x n)
    (cond ((> d dmax) (display "; done") (newline))
          ((> x xmax) (display "; ") (newline)
                      (iter (dinc d) xmin nmin))
          ((> n nmax) (iter d (xinc x) nmin))
          ((nth-root-count-steps d x n first-guess max-steps)
           (iter d x (ninc n)))
          (else (iter (dinc d) xmin nmin))))
  (iter dmin xmin nmin))

(define (nth-root-count-steps d x n first-guess max-steps)
  (let ((result (fixed-point-count-steps ((repeated average-damp d) (f x n))
                                         first-guess
                                         max-steps)))
    (display "; ")
    (display (list 'nth-root-count-steps d x n first-guess max-steps))
    (display " => ")
    (display (if result result "doesn't converge"))
    (newline)
    result))

#| xinc is for iterating over x values. Skip x=1 as it's a trivial case. |#

(define (xinc x)
  (if (< (abs (- x 1e-3)) 1e-6)
      (* x 1e6)
      (* x 1e3)))

#| Now we run the test procedure. |#

(nth-root-test 2 8 inc 1e-12 1e12 xinc 5 10 inc 1.0 1000))

  ; (nth-root-count-steps 2 .000000000001 5 1. 1000) => 22
  ; (nth-root-count-steps 2 .000000000001 6 1. 1000) => 23
  ; (nth-root-count-steps 2 .000000000001 7 1. 1000) => 20
  ; (nth-root-count-steps 2 .000000000001 8 1. 1000) => doesn't converge
  ;
  ; (nth-root-count-steps 3 .000000000001 5 1. 1000) => 47
  ; (nth-root-count-steps 3 .000000000001 6 1. 1000) => 40
  ; ...
  ; (nth-root-count-steps 3 1000.0000000000002 5 1. 1000) => 40
  ; (nth-root-count-steps 3 1000.0000000000002 6 1. 1000) => 38
  ; ...
  ; (nth-root-count-steps 3 1000000000.0000002 9 1. 1000) => 130
  ; (nth-root-count-steps 3 1000000000.0000002 10 1. 1000) => 132
  ;
  ; (nth-root-count-steps 4 .000000000001 5 1. 1000) => 96
  ; (nth-root-count-steps 4 .000000000001 6 1. 1000) => 82
  ; ...
  ; (nth-root-count-steps 4 1000.0000000000002 5 1. 1000) => 72
  ; (nth-root-count-steps 4 1000.0000000000002 6 1. 1000) => 70
  ; ...
  ; (nth-root-count-steps 4 1000000000.0000002 9 1. 1000) => 258
  ; (nth-root-count-steps 4 1000000000.0000002 10 1. 1000) => 260
  ; ...

#| We can see from running the test procedure that, for the range of x and n
tested, the ideal number of average damps d is 3. Using d=2 results in at
least one result that doesn't converge (or converges very slowly), and using d=4
or greater results in consistently slower executions across the entire range of
input values. Thus, we implement nth-root using 3 average damps. |#

(define (nth-root x n)
  (fixed-point ((repeated average-damp 3) (f x n)) 1.0))

;; Exercise 1.46 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve)
         (improve guess)))))

(define (sqrt x)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x)) 0.001))
                      (lambda (guess) (average guess (/ x guess))))
   x))

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (guess)
                        (< (abs (- guess (f guess))) tolerance))
                      f)
   first-guess))
