(ns ray.camera
  (require [ray.geometry :as g]
           [ray.vec :as v]))

(defrecord Camera [eye origin xaxis yaxis])

(defn make-camera [eye target up fov width height]
  (let [image-plane (/ (/ height 2)
                       (Math/tan (/ fov 2)))
        v (v/normalize (v/- target eye))
        xaxis (v/normalize (v/cross v up))
        yaxis (v/normalize (v/cross v xaxis))
        center (v/scale v image-plane)
        origin (v/- center
                    (v/scale xaxis (* 0.5 width))
                    (v/scale yaxis (* 0.5 height)))]
    (->Camera eye origin xaxis yaxis)))

(defn ray [{:keys [eye origin xaxis yaxis] :as camera} x y]
  (g/->Ray eye
           (v/normalize
            (v/+ origin
                 (v/scale xaxis x)
                 (v/scale yaxis y)))))
