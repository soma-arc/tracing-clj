(ns ray.scene
  (require [ray.vec :as v]
           [ray.spectrum :as s]
           [ray.geometry :as g]))

(defrecord Scene [obj-list light-list])

(defn add-obj [scene obj]
  (assoc scene :obj-list (cons obj (:obj-list scene))))

(defn add-light [scene light]
  (assoc scene :light-list (cons light (:light-list scene))))

(defn find-nearest-intersection [{:keys [obj-list] :as scene} ray]
  (let [isect (g/->Intersection g/+no-hit+ nil nil nil)]
    (loop [i 0
           isect isect]
      (if (< i (count obj-list))
        (recur (inc i) (g/intersect (nth obj-list i) isect ray))
        isect))))

(defn diffuse-lighting [p n diffuse-color light-pos light-power]
  (let [v (v/- light-pos p)
        l (v/normalize v)
        dot (v/dot n l)]
    (if (> dot 0)
      (let [r (v/length v)
            factor (/ dot (* 4 (Math/PI) r r))]
        (s/* (s/scale light-power factor)
             diffuse-color))
      s/+black+)))

(defn lighting [{:keys [light-list] :as scene} p n material]
  (let [l s/+black+]
    (loop [i 0
           l l]
      (if (< i (count light-list))
        (let [{light-pos :pos light-power :power} (nth light-list i)]
          (recur (inc i) (s/+ l (diffuse-lighting p
                                                  n
                                                  (:color material)
                                                  light-pos
                                                  light-power))))
        l))))

(defn trace [scene ray]
  (let [isect (find-nearest-intersection scene ray)]
    (if (g/hit isect)
      (lighting scene (:p isect) (:n isect) (:material isect))
      s/+black+)))
