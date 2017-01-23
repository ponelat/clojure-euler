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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn euler-1 
  "The sum of all multiples of 5 and 3, under 1000"
  [] 
  (sum-of-5s-and-3s-below 1000))

(defn euler-2
  "Even Fibonacci numbers"
  []
  (reduce + (filter odd? (fibs-up-to 4000000))))
  
