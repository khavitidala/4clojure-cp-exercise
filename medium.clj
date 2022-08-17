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