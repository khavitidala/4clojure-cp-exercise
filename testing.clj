(ns testing)

(defn sum [x s]
  (+ x s))

(defn penultimate [lst]
  (nth lst (- (count lst) 2)))

(defn fib [x]
  (if (= x 1)
    1
    (if (= x 2)
      1
      (+ (fib (- x 1))
         (fib (- x 2))))))

(defn fib' [n]
  (if (= n 1)
    [1]
    (if (= n 2)
      [1]
      (loop [j [1 1]]
        (if (>= (count j) n)
          j
          (recur (conj j (+' (nth j (- (count j) 2)) (last j)))))))))

(defn conv2 [x]
  (->> x
       (map-indexed (fn [_ itm] {:typ (str (type itm)) :valu itm}))
       (into [])
       (sort-by (juxt :typ :val))
       (map-indexed (fn [_ k] {(k :typ) (k :valu)}))
       (into [])
       (#(let [k (into [] (doall (distinct (apply concat (map keys %)))))] 
           (map (fn [l] (->> %
                             ((fn [x] (filter (fn [m] (= (first (keys m)) l)) x)))
                             (map vals)
                             (apply concat)
                             (into []))) k)))
       (into []))
  )

(def tmp (conv2 [[1 2] :a [3 4] 5 6 :b]))

(def kk (into [] (doall (distinct (apply concat (map keys tmp))))))

(map (fn [l] (->> tmp
                  ((fn [x] (filter (fn [m] (= (first (keys m)) l)) x)))
                  (map vals)
                  (apply concat)
                  (into []))) kk)

(into [] (apply concat (map vals (doall (filter (fn [k] (= (first (keys k)) (kk 0))) tmp)))))

