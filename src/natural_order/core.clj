(ns natural-order.core)

(def factorials (let [f (fn [[n m]] [(* n m) (+ m 1)])]
                  (map first (iterate f [1 2]))))
(defn factorial [n] (nth factorials (- n 1)))

(def naturals (iterate inc 1))
(def nonnegatives (iterate inc 0))
  
(defn choice-sequence->number
  "Given a valid sequence of choices (not including the terminating zero) and its length, reduce it to a number. For instance, (4 3 2 1), representing always-last-element choice from a sequence of 5 elements, returns 119, the largest number obtainable with such a sequence. (1 0 0 0) returns 24."
  [xs]
  (apply + (map * (reverse xs) factorials)))

(defn number->choice-sequence
  "Given a number and a sequence length, encode it as a choice sequence (one shorter than the sequence length)."
  [n l]
  (when (>= n (factorial l))
    (throw (new java.lang.RuntimeException "sequence too short")))
  (if (= l 1)
    nil
    (cons (quot n (factorial (- l 1)))
          (number->choice-sequence (rem n (factorial (- l 1))) (- l 1)))))

(defn tails
  [xs]
  (take-while seq (iterate rest xs)))

(defn index-of
  ([x xs]
     (index-of x xs 0))
  ([x xs n]
     (if (= x (first xs))
       n
       (recur x (rest xs) (inc n)))))
  
(defn choice-of
  "Taking a sortable sequence of unique values, return the index of the first in the corresponding sorted sequence."
  [xs]
  (index-of (first xs) (sort xs)))

(defn sortable-sequence->choice-sequence
  "Taking a sortable sequence of unique values, convert it to a choice sequence."
  [xs]
  (let [sxs (sort xs)]
    (take (- (count xs) 1) (map choice-of (tails xs)))))

(defn choice-sequence->sortable-sequence
  [cs ss]
  (let [f (fn [cs ss]
            (if (nil? (seq cs))
              (list (first ss))
              (let [a (nth ss (first cs))
                    rss (remove (fn [x] (= x a)) ss)]
                (cons a
                      (choice-sequence->sortable-sequence (rest cs) rss)))))]
    (f cs (sort ss))))
    
    
