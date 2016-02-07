(ns ray.geometry
  (require [ray.vec :as v]
           [ray.spectrum :as s]
           [quil.core :as q]))

(defrecord Ray [origin dir])
(defrecord Material [color reflection])
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

(defrecord CheckedObj [obj inv-grid-size material2]
  Intersectable
  (intersect [this isect {ray-origin :origin ray-dir :dir :as ray}]
    (let [[intersect? {:keys [p] :as isect}] (intersect obj isect ray)]
      (if intersect?
        (let [i (+ (Math/round (* (:x p) inv-grid-size))
                   (Math/round (* (:y p) inv-grid-size))
                   (Math/round (* (:z p) inv-grid-size)))]
          [true (if (not= 0 (mod i 2))
                  (assoc isect :material material2)
                  isect)])
        [false isect]))))
