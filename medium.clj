;;
;; 16 Aug 22
;;
;; ==============
;; > Reverse Interleave
;;   problem/43
;; solution 1
(fn [x y]
  (let [res '()]
    (for [i (range 1 (+ y 1))]
      (flatten (cons res (for [j (take (quot (count x) y) (iterate (partial + y) i))]
                           (nth x (dec j))))))))
;; solution 2
#(apply map list (partition %2 %1))
;; ==============
;; > Rotate Sequence
;;   problem/44
;; solution 1
(fn [x y]
  (let [len (count y)
        z (rem x len)]
    (flatten (if (neg? z)
               (cons (take-last (* -1 z) y) (take (+ (count y) z) y))
               (cons (take-last (- (count y) z) y) (take z y))))))
;; solution 2
(fn [n l]
  (->> (split-at (mod n (count l)) l)
       reverse
       (apply concat)))
;; solution 3
(fn [x lst]
  (->> (cycle lst)
       (drop (mod x (count lst)))
       (take (count lst))))
;; ==============
;; > Partition a Sequence
;;   Special Restrictions
;;   partition
;;   partition-all
;;   problem/54
;; solution 1
(fn [x y]
  (->> (map #(take x (range % (count y))) y)
       (take-nth x)
       (take-while #(= (count %) x))))
;; solution 2
(fn [n xs]
  (loop [lst [] a xs]
    (if (< (count a) n)
      lst
      (recur (conj lst (take n a)) (drop n a)))))
;; ==============
;;
;; 18 Aug 22
;;
;; ==============
;; > To Tree, or not to Tree
;;   problem/95
;; solution 1
(fn [x]
  (->> (flatten x)
       (map #(if (not (nil? %)) 1 0))
       (apply +)
       (#(- (- (count (flatten x)) %) %))
       (= 1)))
;; solution 2
(fn f [t]
  (or (nil? t)
      (and (coll? t)
           (= 3 (count t))
           (every? f (rest t)))))
;; ==============
;; > Recognize Playing Cards
;;   problem/128
;; solution 1
(fn [x]
  (let [kind {"S" :spade, "H" :heart, "D" :diamond, "C" :club}
        rank-type [2 3 4 5 6 7 8 9 "T" "J" "Q" "K" "A"]
        rank (into (sorted-map) (map-indexed (fn [idx itm] [(str itm) idx]) rank-type))]
    {:suit (kind (str (get x 0))) :rank (rank (str (get x 1)))}))
;; solution 2
(fn [[a b]]
  (let [s (zipmap "DHCS" [:diamond :heart :club :spade])
        r (zipmap "23456789TJQKA" (range 13))]
    {:suit (s a) :rank (r b)}))
;; ==============
;; > Pairwise Disjoint Sets
;;   problem/153
;; solution 1
(fn [x]
  (let [lst (into [] x)]
    (->> (for [i lst]
           (->> (map #(if (not (= i %)) (= i (apply disj i %)) nil) lst)
                (filter #(not (nil? %)))))
         flatten
         (every? true?))))
;; solution 2
(fn [sets]
  (= (reduce + (map count sets))
     (count #_{:clj-kondo/ignore [:unresolved-namespace]}
      (reduce clojure.set/union sets))))
;; ==============
;; > Beauty is Symmetry
;;   problem/96
;; solution 1
(fn [y]
  (let [get-tree (fn [x] (->> (tree-seq coll? seq x)
                              rest
                              (filter coll?)
                              (take-last 2)))
        left (second y)
        right (last y)
        valid (not (or (nil? left) (nil? right)))]
    (if valid
      (or (= (get-tree left) (reverse (get-tree right)))
          (= left right))
      false)))
;; solution 2
#(= ((fn mirror [[n l r :as tree]] (when tree [n (mirror r) (mirror l)])) %) %)
;; ==============
;; > Count Occurences
;;   problem/55
;; solution 1
(fn [x]
  (->> (group-by identity x)
       (map (fn [k] [(key k) (count (val k))]))
       (into {})))
;; ==============
;;
;; 19 Aug 22
;;
;; ==============
;; > Find Distinct Items
;;   Special Restrictions
;;   distinct
;;   problem/56
;; solution 1
(fn [x]
  (let [result (fn res [y z]
                 (if (empty? y) 
                   z 
                   (if (true? (some #(= (first y) %) z)) 
                     (res (rest y) z)
                     (res (rest y) (conj z (first y))))))]
    (result x [])))
;; solution 2
(fn [x] (reduce #(if ((set %1) %2) %1 (conj %1 %2)) [] x))
;; ==============
;; > Prime Numbers
;;   problem/67
;; solution 1
(fn [x]
  (take x 
        (reduce 
         (fn [primes number] 
           (if (some zero? (map (partial mod number) primes)) 
             primes 
             (conj primes number))) 
         [2] 
         (take (* x x) (iterate inc 3)))))
;; ==============
;; > Perfect Numbers
;;   problem/80
;; solution 1
(fn [bil]
  (true? (some (fn [x] (= x bil)) (map #(apply + (range %)) (range 2 bil)))))
;; ==============