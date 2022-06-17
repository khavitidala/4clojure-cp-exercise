;; Double Down
#(* 2 %)

;; Hello World
#(str "Hello, " % "!")

;; factorial using recur
(defn fac [x]
  (loop [n 1
         res 1]
    (if (> n x)
      res
      (recur (inc n) (* res n)))))

;; factorial using apply
#(apply * (range 1 (inc %)))

;; Compress a Sequence
#(apply concat (map distinct (partition-by identity %)))
#(map first (partition-by identity %))
;; Compress a Sequence can also using dedupe

;; A nil key
;; Write a function which, given a key and map, returns true iff the map contains an entry with that key and its value is nil.
#(and (some? (find %2 %1)) (nil? (get %2 %1)))

;; Map Defaults
;; When retrieving values from a map, you can specify default values in case the key is not found: (= 2 (:foo {:bar 0, :baz 1} 2)) 
;; However, what if you want the map itself to contain the default values? Write a function which takes a default value and a sequence of keys and constructs a map.
(#(zipmap %2 (take (count %2) (cycle [%1]))) 0 [:a :b :c])