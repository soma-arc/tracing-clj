(ns ray.vec)
(in-ns 'ray.vec)

(defrecord Vec [x y z])

(defn scale [v scale]
  (apply ->Vec (for [key (keys v)] (* (key v) scale))))

(defn + [arg & vec-list]
  (let [vec-list (cons arg vec-list)]
    (apply ->Vec
           (for [key (keys arg)]
             (loop [v (first vec-list)
                    rest-vec (rest vec-list)
                    acc 0]
               (if (map? v)
                 (recur (first rest-vec)
                        (rest rest-vec)
                        (clojure.core/+ acc (key v)))
                 acc))))))

(defn - [arg & vec-list]
  (if (nil? vec-list)
      (scale arg -1)
      (let [vec-list (cons arg vec-list)]
        (apply ->Vec
               (for [key (keys arg)]
                 (loop [v (second vec-list)
                        rest-vec (nthrest vec-list 2)
                        acc (key (first vec-list))]
                   (if (map? v)
                     (recur (first rest-vec)
                            (rest rest-vec)
                            (clojure.core/- acc (key v)))
                     acc)))))))

(defn length [v]
  (Math/sqrt (reduce clojure.core/+
                     (for [key (keys v)] (* (key v) (key v))))))

(defn normalize [v]
  (scale v (/ 1 (length v))))

(defn dot [v1 v2]
  (reduce clojure.core/+
          (for [key (keys v1)]
            (* (key v1) (key v2)))))

(defn cross [v1 v2]
  (apply ->Vec
         (let [keys (cycle [:x :y :z])]
           (for [i (range 3)]
             (let [key1 (nth keys (clojure.core/+ i 1))
                   key2 (nth keys (clojure.core/+ i 2))]
               (clojure.core/- (* (key1 v1) (key2 v2))
                               (* (key1 v2) (key2 v1))))))))
