(ns euler.core)

(defn filter-multiple-of
  [num]
  (fn [x] (= (mod x num) 0)))

(defn sum-of-5s-and-3s-below
  [limit]
  (reduce + (filter  
              (fn [x] (or (= (mod x 5) 0) 
                          (= (mod x 3) 0)))
              (range limit))))

(defn fibs-up-to
  [x]
  (letfn
    [
      (next-fib
        [coll]
        (conj coll (+ (first coll) (second coll))))

      (more-fibs 
        [coll]
        (if (> (first coll) x) 
          (rest coll)
          (more-fibs (next-fib coll))))] 

   (more-fibs '(1 1))))

(defn filter-out-first
  [coll]
  (let [factor (first coll)]
    (filter (fn [x] (not= (mod x factor) 0)) coll))) 

(defn reduce-coll-to-highest-prime-factor
  [coll]
  (if (= (count coll) 1)
    (first coll)
    (reduce-coll-to-highest-prime-factor (filter-out-first coll))))

(defn is-prime
  [primes x]
  (not (some (fn [prime] (= (mod x prime) 0)) primes)))

(defn more-primes
  ([] '(2))
  ([primes] 
   (if (<= (first primes) 2)
    '(3 2)
     (more-primes primes (+ 2 (first primes)))))
  ([primes x]
   (if (is-prime primes x)
      (conj primes x)
      (more-primes primes (+ 2 x)))))

(defn factorize
  ([x] (factorize (more-primes) x))
  ([primes x] (factorize primes x '()))
  ([primes x factors]
   (if (= x 1)
     factors
     (let [next-prime (first primes)]
      (if (= (mod x next-prime) 0)
       (factorize primes (/ x next-prime) (conj factors next-prime))
       (factorize (more-primes primes) x factors))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn euler-1 
  "The sum of all multiples of 5 and 3, under 1000"
  [] 
  (sum-of-5s-and-3s-below 1000))

(defn euler-2
  "Even Fibonacci numbers"
  []
  (reduce + (filter odd? (fibs-up-to 4000000))))

(defn euler-3
  "Highest prime factor of x"
  []
  (first (factorize 600851475143)))
  
