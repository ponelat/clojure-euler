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
   (if (<= x 1)
     factors
     (let [next-prime (first primes)]
      (if (= (mod x next-prime) 0)
       (factorize primes (/ x next-prime) (conj factors next-prime))
       (factorize (more-primes primes) x factors))))))

(defn less-than
  [x]
  (fn [y] (< y x)))

(def three-digit-numbers (drop-while (less-than 100) (range 1000)))

(defn build-matrix
  [coll-a coll-b f]
  (letfn [
          (make-col [f x coll]
           (map (partial f x) coll))
          (make-matrix [coll-a coll-b f]
           (map (fn [x] (make-col f x coll-a)) coll-b))]
    (make-matrix coll-a coll-b f)))

(defn flat-set
  [matrix]
  (set (flatten matrix)))

(def products-of-three-digits (flat-set (build-matrix three-digit-numbers three-digit-numbers *)))  

(defn is-palindrome
  [x]
  (let [strx (str x)]
   (loop [start 0 end (- (count strx) 1)]
    (if (>= start end) true
     (if (= (nth strx start) (nth strx end)) 
      (recur (+ start 1) (- end 1))
      false)))))

(def palindromes-three-digit-factors (filter is-palindrome products-of-three-digits))

(defn filter-by
  ([coll] coll)
  ([x coll]
   (filter #(= x %) coll))) 

(defn build-number
  "Builds a number from a map, where the key is the factor and the value is the power"
  [m]
  (reduce 
    (fn [acc [k v]] (* acc (int (Math/pow k v))))
    1
    m))

(def square #(* % %))

(defn sum-of-squares
  [limit]
  (apply + (map square (range (+ 1 limit)))))

(defn square-of-sum
  [limit]
  (square (apply + (range (+ 1 limit)))))


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

(defn euler-4
  "Highest palindrome number with two, three-digit factors"
  []
  (first (reverse (sort palindromes-three-digit-factors))))

(defn euler-5
  "Smallest number, evenly divisible by the numbers 1-20"
  []
  (let [ factors-seq (map factorize (range 20)) 
         map-of-factors 
         (reduce 
           (fn [a b] (merge-with #(max %1 %2) a (frequencies b))) 
           (frequencies (first factors-seq))
           (rest factors-seq))]
   (build-number map-of-factors)))

(defn euler-6
  "difference between sum of squares and square of sums, for first 100 natural numbers"
  []
  (- (square-of-sum 100N) (sum-of-squares 100N)))



