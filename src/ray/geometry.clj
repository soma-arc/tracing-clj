(ns ray.geometry
  (require [ray.vec :as v]
           [ray.spectrum :as s]))

(defrecord Ray [origin dir])
(defrecord Material [color reflection refraction refractive-index emissive-color])

(defrecord Light [pos power])

(def +vacuum-refractive-index+ 1.0)

(def +no-hit+ (Float/POSITIVE_INFINITY))
(def +epsilon+ 0.001)

(defrecord Intersection [t p n material])

(defn hit [isect]
  (not= (:t isect) +no-hit+))

(defprotocol Intersectable
  (intersect [this isect ray]))

(defrecord Sphere [center radius material]
  Intersectable
  (intersect [this isect {ray-origin :origin ray-dir :dir :as ray}]
    (let [v (v/- ray-origin center)
          b (v/dot ray-dir v)
          c (- (v/dot v v) (* radius radius))
          d (- (* b b) c)]
      (if (>= d 0)
        (let [s (Math/sqrt d)
              t (- (- b) s)
              t (if (<= t +epsilon+) (- s b) t)]
          (if (and (< +epsilon+ t) (< t (:t isect)))
            (let [p (v/+ ray-origin (v/scale ray-dir t))
                  n (v/normalize (v/- p center))]
              [true (assoc isect
                           :t t
                           :p p
                           :n n
                           :material material)])
            [false isect]))
        [false isect]))))

(def +sphere-pos1+ [5 5 0])
(def +sphere-pos2+ [5 -5 0])
(def +sphere-pos3+ [-5 5 0])
(def +sphere-pos4+ [-5 -5 0])
(def +sphere-pos5+ [0 0 7.071])
(def +sphere-pos6+ [0 0 -0.071])
(def +sphere-r+ 5)
(def +sphere-r2+ 5)
(def +max-klein-loop+ 3)
(def klein-sphere-r 2.0833)

(defn dist-func [center pos]
  (loop [i 0
         pos (v/+ pos center)
         dr 1.]
    (if (> i +max-klein-loop+)
      [(* 0.08
          (/ (- (v/length pos) klein-sphere-r)
             (Math/abs dr))) (dec i)]
      (cond
        (< (v/length (v/- pos +sphere-pos1+)) +sphere-r+)
        (let [diff (v/- pos +sphere-pos1+)
              dr (* dr (/ +sphere-r2+ (v/dot diff diff)) )
              pos (v/+ (v/scale (v/scale diff +sphere-r2+)
                                (/ 1 (* (v/length diff) (v/length diff))))
                       +sphere-pos1+)]
          (recur (inc i) pos dr))
        (< (v/length (v/- pos +sphere-pos2+)) +sphere-r+)
        (let [diff (v/- pos +sphere-pos2+)
              dr (* dr (/ +sphere-r2+ (v/dot diff diff)) )
              pos (v/+ (v/scale (v/scale diff +sphere-r2+)
                                (/ 1 (* (v/length diff) (v/length diff))))
                       +sphere-pos2+)]
          (recur (inc i) pos dr))
        (< (v/length (v/- pos +sphere-pos3+)) +sphere-r+)
        (let [diff (v/- pos +sphere-pos3+)
              dr (* dr (/ +sphere-r2+ (v/dot diff diff)) )
              pos (v/+ (v/scale (v/scale diff +sphere-r2+)
                                (/ 1 (* (v/length diff) (v/length diff))))
                       +sphere-pos3+)]
          (recur (inc i) pos dr))
        (< (v/length (v/- pos +sphere-pos4+)) +sphere-r+)
        (let [diff (v/- pos +sphere-pos4+)
              dr (* dr (/ +sphere-r2+ (v/dot diff diff)) )
              pos (v/+ (v/scale (v/scale diff +sphere-r2+)
                                (/ 1 (* (v/length diff) (v/length diff))))
                       +sphere-pos4+)]
          (recur (inc i) pos dr))
        (< (v/length (v/- pos +sphere-pos5+)) +sphere-r+)
        (let [diff (v/- pos +sphere-pos5+)
              dr (* dr (/ +sphere-r2+ (v/dot diff diff)) )
              pos (v/+ (v/scale (v/scale diff +sphere-r2+)
                                (/ 1 (* (v/length diff) (v/length diff))))
                       +sphere-pos5+)]
          (recur (inc i) pos dr))
        (< (v/length (v/- pos +sphere-pos6+)) +sphere-r+)
        (let [diff (v/- pos +sphere-pos6+)
              dr (* dr (/ +sphere-r2+ (v/dot diff diff)) )
              pos (v/+ (v/scale (v/scale diff +sphere-r2+)
                                (/ 1 (* (v/length diff) (v/length diff))))
                       +sphere-pos6+)]
          (recur (inc i) pos dr))
        :default [(* 0.08
                      (/ (- (v/length pos) klein-sphere-r)
                         (Math/abs dr)))
                  i]))))

(defn get-normal [center p]
  (v/normalize (v/->Vec (- (first (dist-func center (v/+ p (v/->Vec 0.01 0 0))))
                           (first (dist-func center (v/- p (v/->Vec 0.01 0 0)))))
                        (- (first (dist-func center (v/+ p (v/->Vec 0 0.01 0))))
                           (first (dist-func center (v/- p (v/->Vec 0 0.01 0)))))
                        (- (first (dist-func center (v/+ p (v/->Vec 0 0 0.01))))
                           (first (dist-func center (v/- p (v/->Vec 0 0 0.01))))))))

(def +max-marching-loop+ 100)
(defrecord Klein [center material]
  Intersectable
  (intersect [this isect {ray-origin :origin ray-dir :dir :as ray}]
    (loop [i 0
           dist 0
           ray-pos ray-origin
           ray-length 0]
      (if (>= i +max-marching-loop+)
        [false isect]
        (let [dist (first (dist-func center ray-pos))
              ray-length (+ ray-length dist)
              ray-pos (v/+ ray-origin (v/scale ray-dir ray-length))]
          (if (< dist 0.001)
            [true (assoc isect
                         :t ray-length
                         :p ray-pos
                         :n (get-normal center ray-pos)
                         :material material)]
            (recur (inc i) dist ray-pos ray-length)))))))

(defrecord Plane [n d material]
  Intersectable
  (intersect [this isect {ray-origin :origin ray-dir :dir :as ray}]
    (let [v (v/dot n ray-dir)
          t (if (< (Math/abs v) +epsilon+)
              +no-hit+
              (/ (- (+ (v/dot n ray-origin)
                       d))
                 v))]
      (if (and (< +epsilon+ t) (< t (:t isect)))
        [true (assoc isect
                     :t t
                     :p (v/+ ray-origin (v/scale ray-dir t))
                     :n n
                     :material material)]
        [false isect]))))

(defn make-plane [p n material]
  (->Plane (v/normalize n)
           (- (v/dot p n))
           material))

(defrecord CheckedObj [obj inv-grid-size material2]
  Intersectable
  (intersect [this isect {ray-origin :origin ray-dir :dir :as ray}]
    (let [[intersect? {:keys [p] :as isect}] (intersect obj isect ray)]
      (if intersect?
        (let [i (+ (Math/round (* (:x p) inv-grid-size))
                   (Math/round (* (:y p) inv-grid-size))
                   (Math/round (* (:z p) inv-grid-size)))]
          [true (if (not= 0 (rem i 2))
                  (assoc isect :material material2)
                  isect)])
        [false isect]))))
