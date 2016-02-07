(ns ray.core
  (require [ray.vec :as v]
           [ray.spectrum :as s]
           [quil.core :as q]
           [quil.middleware :as m]
           [ray.geometry :as g]
           [ray.scene :as scene]))

(in-ns 'ray.core)

(def eye (v/->Vec 0 0 6))

(defn calc-primary-ray [x y]
  (let [image-plane (q/height)
        ray-dir (v/normalize
                 (v/->Vec
                  (+ x 0.5 (- (/ (q/width) 2)))
                  (- (+ y 0.5 (- (/ (q/height) 2))))
                  (- image-plane)))]
    (g/->Ray eye ray-dir)))

(defn calc-pixel-color [x y scene]
  (let [ray (calc-primary-ray x y)
        l (scene/trace scene ray)]
    (apply q/color (s/->color l))))

(defn setup []
  {:y 0
   :scene (let [scene (scene/->Scene [] [])]
            (-> scene
                (scene/add-obj (g/->Sphere (v/->Vec -2.1 0 0) 1
                                           (g/->Material (s/->Spectrum 0.9 0.1 0.9))))
                (scene/add-obj (g/->Sphere (v/->Vec 0 0 0) 1
                                           (g/->Material (s/->Spectrum 0.9 0.9 0.1))))
                (scene/add-obj (g/->Sphere (v/->Vec 2.1 0 0) 1
                                           (g/->Material (s/->Spectrum 0.1 0.9 0.9))))
                (scene/add-obj (g/make-plane (v/->Vec 0 -1 0) (v/->Vec 0 1 0)
                                             (g/->Material (s/->Spectrum 0.9 0.9 0.9))))
                (scene/add-light (g/->Light (v/->Vec 1000 1000 1000)
                                            (s/->Spectrum 40000000 40000000 40000000)))))})

(defn update-state [state]
  (assoc state :y (inc (:y state))))

(defn draw [{:keys [y scene] :as state}]
  (doseq [x (range (q/width))]
    (let [c (calc-pixel-color x y scene)]
      (q/set-pixel x y c)))
  (if (>= y (q/height))
    (q/no-loop)))

(defn key-pressed [state event]
  (q/start-loop)
  (assoc state :y 0))

(defn key-released [state]
  state)


(q/defsketch ray
  :title "cast"
  :setup setup
  :draw draw
  :update update-state
  :key-pressed key-pressed
  :key-released key-released
  :features [:size [640 480]
             :resizable true]
  :middleware [m/fun-mode m/pause-on-error])
