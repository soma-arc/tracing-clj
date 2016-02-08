(ns ray.scene
  (require [ray.vec :as v]
           [ray.spectrum :as s]
           [ray.geometry :as g]))

(def +max-depth+ 10)

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
        (recur (inc i) (second (g/intersect (nth obj-list i) isect ray)))
        isect))))

(defn visible? [{:keys [obj-list] :as scene} org target]
  (let [v (v/- target org)
        shadow-ray (g/->Ray org (v/normalize v))
        isect (g/->Intersection (v/length v) nil nil nil)
        obj-num (count obj-list)]
    (loop [i 0]
      (if (< i obj-num)
        (if (first (g/intersect (nth obj-list i) isect shadow-ray))
          false
          (recur (inc i)))
        true))))

(defn diffuse-lighting [scene p n diffuse-color light-pos light-power]
  (let [v (v/- light-pos p)
        l (v/normalize v)
        dot (v/dot n l)]
    (if (and (> dot 0) (visible? scene p light-pos))
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
          (recur (inc i) (s/+ l (diffuse-lighting scene
                                                  p
                                                  n
                                                  (:color material)
                                                  light-pos
                                                  light-power))))
        l))))


(defn reflect [v n]
  (v/- v (v/scale n (* 2 (v/dot v n)))))

(defn refract [v n eta-i eta-t]
  (let [dot (v/dot v n)
        eta (/ eta-i eta-t)
        d (- 1 (* eta eta
                  (- 1 (* dot dot))))]
    (if (> d 0)
      (v/- (v/scale (v/- v (v/scale n dot)) eta)
           (v/scale n (Math/sqrt d)))
      nil)))

(defn trace [scene {ray-dir :dir :as ray} depth]
  (letfn [(trace-reflection [scene p n v depth]
            (trace scene (g/->Ray p (reflect v n)) depth))
          (trace-refraction [scene p n v eta-i eta-t depth]
            (let [r (refract v n eta-i eta-t)]
              (if (not= r nil)
                (trace scene (g/->Ray p r) depth)
                (trace-reflection scene p n v depth))))]
    (let [{material :material
           isect-p :p
           isect-n :n :as isect} (find-nearest-intersection scene ray)
          depth (inc depth)]
      (if (or (not (g/hit isect))
              (> depth +max-depth+))
        s/+black+
        (if (< (v/dot isect-n ray-dir) 0)
          (let [l s/+black+
                ks (:reflection material)
                l (if (> ks 0)
                    (s/+ l (s/* (s/scale (trace-reflection scene isect-p isect-n ray-dir depth)
                                         ks)
                                (:color material)))
                    l)
                kt (:refraction material)
                l (if (> kt 0)
                    (s/+ l (s/* (s/scale (trace-refraction scene isect-p isect-n ray-dir
                                                           g/+vacuum-refractive-index+
                                                           (:refractive-index material)
                                                           depth)
                                         kt)
                                (:color material)))
                    l)
                kd (- 1.0 ks kt)
                l (if (> kd 0)
                    (s/+ l (s/scale (lighting scene isect-p isect-n material) kd))
                    l)]
            l)
          (trace-refraction scene isect-p (v/- isect-n) ray-dir
                            (:refractive-index material)
                            g/+vacuum-refractive-index+
                            depth))))))
