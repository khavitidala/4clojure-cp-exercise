;; Duplicate a Sequence
;; Write a function which duplicates each element of a sequence.
#(apply concat (map (fn [x] (repeat 2 x)) %))
#(interleave % %)

;; Replicate a Sequence
;; Write a function which replicates each element of a sequence a variable number of times.
(#(apply concat (map (fn [x] (repeat %2 x)) %1)) [1 2 3] 2)
#(apply interleave (repeat %2 %1))

;; Implement range
;; Write a function which creates a list of all integers in a given range.
;; Special Restrictions range
(#(loop [res [%1]
         min (inc %1)]
    (if (>= (last res) (dec %2))
      res
      (recur (conj res min) (inc min)))) -2 2) ;; [-2 -1 0 1]
#(take (- %2 %1) (iterate inc %1))

;; Interpose a Seq
;; Write a function which separates the items of a sequence by an arbitrary value.
((fn [k l] (butlast (apply concat (map (fn [x] (reduce #(conj %1 %2) [x] [k])) l)))) ", " ["one" "two" "three"])
#(butlast (interleave %2 (repeat %)))

;; Interleave Two Seqs
;; Write a function which takes two sequences and returns the first item from each, 
;; then the second item from each, then the third, etc.
;; Special Restrictions interleave
((fn [a b] (let [ca (count a)
                 cb (count b)]
             (flatten (for [n (range 0 (if (< ca cb) ca cb))
                            :let [x a]
                            :let [y b]]
                        [(get x n) (get y n)])))) [1 2 3 4] [5])
(fn [a b] (mapcat #(vector % %2) a b))

;; Drop Every Nth Item
;; Write a function which drops every Nth item from a sequence.
((fn [x y] (->> x
                (partition y)
                (mapcat butlast)
                (conj (drop (- (count x) (mod (count x) y)) x))
                flatten)) [1 2 3 4 5 6 7 8] 3)
#(flatten (partition-all (dec %2) %2 %))

;; Map Construction
;; Write a function which takes a vector of keys and a vector of values and constructs a map from them.
;; Special Restrictions zipmap
((fn [x y]
   (apply hash-map (#(interleave %1 %2) x y))) [1 2 3 4] ["one" "two" "three"])

;; Greatest Common Divisor
(fn gcd [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))))

;; set intersection
;; Write a function which returns the intersection of two sets. The intersection is the sub-set of items that each set has in common.
;; Special Restrictions intersection
((fn [x y]
   (map first (filter #(> (val %) 1)
                      (frequencies (concat (into [] (into (sorted-set) x))
                                           (into [] (into (sorted-set) y))))))) #{0 1 2 3} #{2 3 4 5})
#_{:clj-kondo/ignore [:unresolved-namespace]}
(clojure.set/select #{0 1 2 3} #{2 3 4 5})

;; A Half-Truth
;; Write a function which takes a variable number of booleans. 
;; Your function should return true if some of the parameters are true, 
;; but not all of the parameters are true. Otherwise your function should return false.
(fn [& xs]
  (let [sq (into [] xs)
        csq (count sq)
        asq (reduce (fn [a b] (and a b)) sq)]
    (if (or (= csq 1) (true? asq))
      (if (true? (first sq)) false false)
      (reduce (fn [a b] (or a b)) sq))))
;; simple solution: not= 

;; Cartesian Product
;; Write a function which calculates the Cartesian product of two sets.
(fn [d b]
  (into #{} (mapcat #(map-indexed (fn [_ v] [v %]) d) b)))
#(into #{} (for [i %1 j %2] [i j]))

;; Re-implement Iteration
;; Given a side-effect free function f and an initial value x 
;; write a function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.
(take 5 ((fn [f x]
           (let [myit (fn myit
                        ([] (myit x))
                        ([n] (lazy-seq (cons n (myit (f n))))))]
             (myit))) #(* 2 %) 1))
(fn g [f y]
  (cons y (lazy-seq (g f (f y)))))

;; Group a Sequence
;; Given a function f and a sequence s, write a function which returns a map. 
;; The keys should be the values of f applied to each item in s. 
;; The value at each key should be a vector of corresponding items in the order they appear in s.
;; Special Restrictions group-by
((fn [f x]
   (->> x
        (map-indexed (fn [_ v] {(f v) v}))
        conj
        (concat (distinct (map-indexed (fn [_ v] {(f v) []}) x)))
        (apply merge-with merge)
        (#(into {} (for [[k v] %] [k (if (sequential? (first v)) (remove empty? v) v)])))))
 #(> % 5) #{1 3 6 8})
;; {false [1 3], true [6 8]}
#(apply merge-with into (for [v %2] {(% v) [v]}))
(fn [f xs]
  (apply merge-with concat
         (for [x xs]
           {(f x) [x]})))

;; Akar persamaan kuadrat
((fn [[a b c]]
   (let [atas (fn [f]
                (/ (f (- b) (Math/sqrt (- (* b b) (* 4 a c)))) (* 2 a)))
         plus (int (atas +))
         minus (int (atas -))
         nol? (= 0 plus minus)]
     (if (true? nol?) #{} (set [plus minus])))) [1 -1 -6])

;; Sum of square of digits
;; Write a function which takes a collection of integers as an argument. 
;; Return the count of how many elements are smaller than the sum of their squared component digits. 
;; For example: 10 is larger than 1 squared plus 0 squared; whereas 15 is smaller than 1 squared plus 5 squared.

(fn [xs]
  (count (for [x xs
               :let [y (map #(* (Integer/parseInt (str %)) (Integer/parseInt (str %))) (str x))]
               :let [sy (reduce + y)]
               :when (< x sy)]
           x)))

;; Pascal's Triangle
;; Pascal's triangle is a triangle of numbers computed using the following rules: 
;; - The first row is 1. - Each successive row is computed by adding together adjacent numbers in the row above, 
;; and adding a 1 to the beginning and end of the row. Write a function which returns :the nth row of Pascal's Triangle.
((fn [x]
   (if (= x 1)
     [1]
     (loop [p 0
            res [1 1]]
       (if (= p (dec (dec x)))
         res
         (recur (inc p) (into [] (concat [1] (for [m (range x)
                                                   :when (and (> m 0) (< m (count res)))]
                                               (+ (get res m) (get res (dec m)))) [1]))))))) 11)
;; [1 10 45 120 210 252 210 120 45 10 1]
(fn [x] (last (take x (iterate #(map + (concat [0] %) (concat % [0])) [1]))))
(fn [n]
  (last (take n (iterate #(map +' `(0 ~@%) `(~@% 0)) [1]))))

;; Least Common Multiple
;; Write a function which calculates the least common multiple. 
;; Your function should accept a variable number of positive integers or ratios.
((fn [& xs]
   (let [cxs (map #(if (ratio? %) (* % (denominator %)) %) xs)
         mxs (reduce * cxs)]
     (->> xs
          (mapcat #(range % (inc mxs) %))
          frequencies
          (into [])
          (sort-by second >)
          ffirst))) 7 5/7 2 3/5)
;; 210

;; Infix Calculator
;; Write a function that accepts a variable length mathematical expression consisting of numbers and the operations +, -, *, and /. 
;; Assume a simple calculator that does not do precedence and instead just calculates left to right.
((fn [& xs]
   (loop [res ((first (next xs)) (first xs) (first (nnext xs)))
          tmp (drop 3 xs)]
     (if (empty? tmp)
       res
       (recur ((first tmp) res (first (next tmp))) (drop 2 tmp))))) 10 / 2 - 1 * 2)
;; 8
(fn [& x]
  (reduce #(let [[a b] %2] (a %1 b))
          (first x)
          (partition 2 (rest x))))

;; Trees into tables
((fn [x] (into {} (flatten (for [m x
                                 :let [valu (into {} (next m))]]
                             (map #(hash-map [%1 (key %2)] (val %2)) (repeat (count valu) (key m)) valu))))) '{a {p 1, q 2}
                                                                                                               b {m 3, n 4}})
;; {[a p] 1, [a q] 2, [b m] 3, [b n] 4}
#(into {} (for [[k v] % [k2 v2] v] [[k k2] v2]))