;; ==============
;;
;; 23 Aug 22
;;
;; ==============
;; > Longest Increasing Sub-Seq
;;   problem/53
;; solution 1
(fn [soal]
  (let [fseq (fn incseq [x stack res]
               (if (false? (empty? x))
                 (if (empty? stack)
                   (incseq (rest x) (conj stack (first x)) res)
                   (if (= (- (first x) (last stack)) 1)
                     (incseq (rest x) (conj stack (first x)) res)
                     (incseq (rest x) (conj [] (first x)) (conj res stack))))
                 (if (empty? stack)
                   res
                   (if (= (- (last stack) (last (last res))) 1)
                     (conj (butlast res) (conj (last res) (last stack)))
                     (conj res stack)))))]
    (->> (fseq soal [] [])
         (apply max-key count)
         (#(if (> (count %) 1) % [])))))