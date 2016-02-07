(ns ray.geometry
  (require [ray.vec :as v]
           [ray.spectrum :as s]))

(defrecord Ray [origin dir])
(defrecord Material [color])
(defrecord Light [pos power])

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

(defrecord Plane [n d material]
  Intersectable
  (intersect [this isect {ray-origin :origin ray-dir :dir :as ray}]
    (let [v (v/dot n ray-dir)
          t (/ (- (+ (v/dot n ray-origin)
                     d))
               v)]
      (if (and (< +epsilon+ t) (< t (:t isect)))
        [true (assoc isect
                 :t t
                 :p (v/+ ray-origin (v/scale ray-dir t))
                 :n n
                 :material material)]
        [false (assoc isect :t +no-hit+)]))))

(defn make-plane [p n material]
  (->Plane (v/normalize n)
           (v/dot (v/- p) n)
            material))
