(ns ray.spectrum)

(defrecord Spectrum [r g b])

(def +display-gamma+ 2.2)

(defn scale [s scale]
  (apply ->Spectrum (for [key (keys s)]
                      (clojure.core/* (key s) scale))))

(defn + [arg & spec-list]
  (let [spec-list (cons arg spec-list)]
    (apply ->Spectrum
           (for [key (keys arg)]
             (loop [v (first spec-list)
                    rest-spec (rest spec-list)
                    acc 0]
               (if (map? v)
                 (recur (first rest-spec)
                        (rest rest-spec)
                        (clojure.core/+ acc (key v)))
                 acc))))))

(defn * [arg & spec-list]
  (let [spec-list (cons arg spec-list)]
    (apply ->Spectrum
           (for [key (keys arg)]
             (loop [v (first spec-list)
                    rest-spec (rest spec-list)
                    acc 1]
               (if (map? v)
                 (recur (first rest-spec)
                        (rest rest-spec)
                        (clojure.core/* acc (key v)))
                 acc))))))

(defn * [s1 s2]
  (apply ->Spectrum (for [key (keys s1)]
                      (clojure.core/* (key s1) (key s2)))))

(defn ->color [s]
  (for [key (keys s)]
    (int (min (clojure.core/* (Math/pow (key s) (/ 1 +display-gamma+))
                              255)
              255))))

(def +black+ (->Spectrum 0 0 0))
