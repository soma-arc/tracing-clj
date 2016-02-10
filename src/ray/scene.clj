(ns ray.scene
  (require [ray.vec :as v]
           [ray.spectrum :as s]
           [ray.geometry :as g]))
(in-ns 'ray.scene)

(def +max-depth+ 10)

(def +sky-color+ (s/->Spectrum 0.8 0.75 0.7))

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

(defn random-dir-over-hemisphere [n]
  (let [[x y z] (loop []
                  (let [x (* 2 (- (Math/random) 0.5))
                        y (* 2 (- (Math/random) 0.5))
                        z (* 2 (- (Math/random) 0.5))
                        ll (+ (* x x)
                              (* y y)
                              (* z z))]
                    (if (< 0.0 ll 1.0)
                      [x y z]
                      (recur))))
        v (v/normalize (v/->Vec x y z))]
    (if (< (v/dot v n) 0)
      (v/- v)
      v)))

(defn path-trace [scene ray depth]
  (letfn [(trace-diffuse-reflection [p n diffuse-color depth]
            (let [r (random-dir-over-hemisphere n)
                  li (path-trace scene (g/->Ray p r) depth)
                  fr (s/scale diffuse-color (/ 1.0 Math/PI))
                  dot (v/dot n r)]
              (s/scale (s/* li fr) (* 2 Math/PI dot))))]
    (if (> (inc depth) +max-depth+)
      s/+black+
      (let [{material :material
             isect-p :p
             isect-n :n :as isect} (find-nearest-intersection scene ray)]
        (if (not (g/hit isect))
          +sky-color+
          (trace-diffuse-reflection isect-p isect-n
                                    (:color material)
                                    depth))))))

(defn path-trace [scene {ray-dir :dir :as ray} depth]
  (letfn [(trace-reflection [scene p n v depth]
            (path-trace scene (g/->Ray p (reflect v n)) depth))
          (trace-refraction [scene p n v eta-i eta-t depth]
            (let [r (refract v n eta-i eta-t)]
              (if (not= r nil)
                (path-trace scene (g/->Ray p r) depth)
                (trace-reflection scene p n v depth))))
          (trace-diffuse-reflection [p n diffuse-color depth]
            (let [r (random-dir-over-hemisphere n)
                  li (path-trace scene (g/->Ray p r) depth)
                  fr (s/scale diffuse-color (/ 1.0 Math/PI))
                  dot (v/dot n r)]
              (s/scale (s/* li fr) (* 2 Math/PI dot))))
          (interact-surface [scene ray-dir p n {material-color :color
                                                ks :reflection
                                                kt :refraction :as m}
                             eta-i eta-t depth]
            (let [kd (- 1 ks kt)
                  t (Math/random)]
              (cond
                (< t ks) (s/* (trace-reflection scene p n ray-dir depth)
                              material-color)
                (< t (+ ks kt)) (s/* (trace-refraction scene p n ray-dir
                                                       eta-i eta-t depth)
                                     material-color)
                :else (trace-diffuse-reflection p n material-color depth))))]
    (if (> (inc depth) +max-depth+)
      s/+black+
      (let [{material :material
             isect-p :p
             isect-n :n :as isect} (find-nearest-intersection scene ray)
             depth (inc depth)]
        (if (not (g/hit isect))
          +sky-color+
          (let [dot (v/dot isect-n ray-dir)]
            (if (< dot 0)
              (let [col (interact-surface scene
                                          ray-dir
                                          isect-p isect-n
                                          material
                                          g/+vacuum-refractive-index+
                                          (:refractive-index material)
                                          depth)]
                (s/+ col (s/scale (:emissive-color material) (- dot))))
              (interact-surface scene
                                ray-dir
                                isect-p
                                (v/- isect-n)
                                material
                                (:refractive-index material)
                                g/+vacuum-refractive-index+
                                depth))))))))

(defn trace [scene {ray-dir :dir :as ray} depth]
  (letfn [(trace-reflection [scene p n v depth]
            (trace scene (g/->Ray p (reflect v n)) depth))
          (trace-refraction [scene p n v eta-i eta-t depth]
            (let [r (refract v n eta-i eta-t)]
              (if (not= r nil)
                (trace scene (g/->Ray p r) depth)
                (trace-reflection scene p n v depth))))]
    (if (> (inc depth) +max-depth+)
      s/+black+
      (let [{material :material
             isect-p :p
             isect-n :n :as isect} (find-nearest-intersection scene ray)
             depth (inc depth)]
        (if (not (g/hit isect))
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
                              depth)))))))
