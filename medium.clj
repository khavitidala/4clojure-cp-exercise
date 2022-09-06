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
(defn genpr [x]
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
;;
;; 22 Aug 22
;;
;; ==============
;; > Filter Perfect Squares
;;   problem/74
;; solution 1
(fn [x]
  (let [lst (->> (clojure.string/split x #",")
                 (map #(Integer/parseInt %)))]
    (->> lst
         (map #(some (fn [x] (= x %)) (map (fn [i] (* i i)) (range %))))
         (keep-indexed #(if (true? %2) (nth lst %1)))
         (clojure.string/join ", ")
         (#(clojure.string/replace % #" " "")))))
;; ==============
;; > Juxtaposition
;;   problem/59
;;   Special Restrictions
;;   juxt
;; solution 1
(fn [& x]
  (fn [& j] (map #(apply % j) x)))
;; ==============
;; > Word Sorting
;;   problem/70
;; solution 1
(fn [x]
  (let [cln (clojure.string/split (clojure.string/replace x #"\.+|\!+|\?+|,+" "") #" ")]
    (->> (map #(.toLowerCase %) cln)
         (map-indexed (fn [idx itm] [(nth cln idx) itm]))
         (sort-by last)
         (map #(first %)))))
;;solution 2
#(sort-by clojure.string/lower-case (re-seq #"\w+" %))
;; ==============
;; > Anagram Finder
;;   problem/77
;; solution 1
(fn [x]
  (->> (group-by sort x)
       vals
       (map #(into #{} %))
       (filter #(> (count %) 1))
       (into #{})))
;; ==============
;; > intoCamelCase
;;   problem/102
;; solution 1
(fn [x]
  (if (nil? (re-find #"-" x))
    x
    (->> (clojure.string/split x #"-")
         ((fn [y]
            (str (first y) (apply str (map #(apply str (.toUpperCase (str (first %))) (rest %)) (rest y)))))))))
;; ==============
;;
;; 23 Aug 22
;;
;; ==============
;; > Function Composition
;;   problem/58
;; solution 1
(fn
  ([f g]
   (fn [& args]
     (f (apply g args))))
  ([f g h]
   (fn [& args]
     (f (g (apply h args))))))
;; solution 2
(fn [& fs] (reduce (fn [f g] #(f (apply g %&))) fs))
;; ==============
;;
;; 24 Aug 22
;;
;; ==============
;; > Euler's Totient Function
;;   problem/75
;; solution 1
(fn [x]
  (let [gcd (fn fpb [a b]
              (if (= a 0)
                b
                (fpb (rem b a) a)))]
    (if (= x 1)
      1
      (->> (map (fn [j]
                  (if (= (gcd j x) 1)
                    j
                    nil)) (range x))
           (filter #(not (nil? %)))
           count))))
;; ==============
;; > Happy numbers
;;   problem/86
;; solution 1
(fn [j]
 (let [hap (fn happy [x y] 
             (if (= y 100) 
               x 
               (happy (->> (str x) 
                           (map #(Integer/parseInt (str %))) 
                           (map #(* % %)) 
                           (apply +)) (inc y))))]
   (= (hap j 1) 1)))
;; ==============
;; > Equivalence Classes
;;   problem/98
;; solution 1
(fn [f d]
  (->> (reduce #(let [res (f %2)]
                  (if (nil? (get %1 res))
                    (assoc %1 res #{%2})
                    (assoc %1 res (conj (get %1  res) %2)))) {} d)
       vals
       (into #{})))
;; solution 2
(fn [f d]
  (->> (group-by f d)
       vals
       (map set)
       set))
;; ==============
;;
;; 25 Aug 22
;;
;; ==============
;; > Sequence Reductions
;;   problem/60
;; solution 1
(fn
  ([f x] (lazy-seq (map #(reduce f (first x) (take (inc %) x)) (range))))
  ([f x y] (lazy-seq (map #(reduce f x (take % y)) (range (inc (count y)))))))
;; ==============
;; > Merge with a Function
;;   problem/69
;;   Special Restrictions
;;   merge-with
;; solution 1
(fn [f & v]
  (let [ky (keys (into {} (merge v)))]
   (->> (map (fn [x] (map #(get % x) v)) ky)
        (map (fn [x] (filter #(false? (nil? %)) x)))
        (map (fn [x] (if (= (count x) 1) (identity (first x)) (apply f x))))
        (map-indexed (fn [idx itm] {(nth ky idx) itm}))
        (into {}))))
;; ==============
;; > The Balance of N
;;   problem/115
;; solution 1
(fn [x]
  (if (< (count (str x)) 2)
    true 
    (->> (str x)
       (map #(Integer/parseInt (str %)))
       ((fn [y] (= (apply + (take (quot (count (str x)) 2) y)) 
                   (apply + (take-last (quot (count (str x)) 2) y))))))))
;; ==============
;;
;; 26 Aug 22
;;
;; ==============
;; > Power Set
;;   problem/85
;; solution 1
(fn [items]
  (reduce
   (fn [s x]
     (clojure.set/union s (map #(conj % x) s)))
   (hash-set #{})
   items))
;; ==============
;;
;; 27 Aug 22
;;
;; ==============
;; > Identify keys and values
;;   problem/105
;; solution 1
(fn [x]
  (if (empty? x)
    {}
    (loop [res {}
           y (first x)
           z (rest x)]
      (if (nil? y)
        res
        (if (keyword? y)
          (recur (assoc res y []) (first z) (rest z))
          (recur
           (assoc
            res
            (last (keys res))
            (if (empty? (get res (last (keys res))))
              [y]
              (conj (get res (last (keys res))) y))) (first z) (rest z)))))))
;; ==============
;;
;; 28 Aug 22
;;
;; ==============
;; > Prime Sandwich
;;   problem/116
;; solution 1
(fn [x]
  (let [genpr (fn [x]
                (reduce
                 (fn [primes number]
                   (if (some zero? (map (partial mod number) primes))
                     primes
                     (conj primes number)))
                 [2]
                 (if (< x 100)
                   (range 3 (* x x))
                   (range 3 (+ x 100)))))
        respr (if (> x 4) (genpr x) '())]
    (if (> x 4)
      (->> respr
           (keep-indexed #(if (= %2 x)
                            (list %2 (nth respr (dec %1)) (nth respr (inc %1)))
                            nil))
           flatten
           (#(if (< (count %) 3) false (= (first %) (quot (apply + (rest %)) 2)))))
      false)))
;; ==============
;;
;; 29 Aug 22
;;
;; ==============
;; > Sum Some Set Subsets
;;   problem/131
;; solution 1
(fn [& x]
  (let [ps (fn [items]
             (reduce
              (fn [s x]
                (clojure.set/union s (map #(conj % x) s)))
              (hash-set #{})
              items))]
    (->> (map #(ps %) x)
         (map (fn [v] (filter #(not (empty? %)) v)))
         (map (fn [v] (map #(apply + %) v)))
         ((fn [v] (map #(some (into #{} (first v)) %) (rest v))))
         (some nil?)
         nil?)))
;; ==============
;;
;; 6 Sep 22
;;
;; ==============
;; > Digits and bases
;;   problem/137
;; solution 1
(fn [x y]
  (if (< x y)
    [0]
   (reverse (loop [bil x
         res []]
    (if (zero? bil)
      res
      (recur (quot bil y) (conj res (rem bil y))))))))