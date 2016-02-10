(ns ray.core
  (require [ray.vec :as v]
           [ray.spectrum :as s]
           [quil.core :as q]
           [quil.middleware :as m]
           [ray.geometry :as g]
           [ray.scene :as scene]
           [ray.camera :as camera]))

(in-ns 'ray.core)

(def eye (v/->Vec 0 2 4))
(def target (v/->Vec 0 0 0))
(def up (v/->Vec 0.5 1 0))
(def fov (Math/toRadians 60))

(defn calc-primary-ray [x y]
  (let [image-plane (q/height)
        ray-dir (v/normalize
                 (v/->Vec
                  (- x (/ (q/width) 2))
                  (- (- y (/ (q/height) 2)))
                  (- image-plane)))]
    (g/->Ray eye ray-dir)))

(def +sample-num+ 16)

(defn calc-pixel-color [x y scene]
  (let [camera (camera/make-camera eye target up fov (q/width) (q/height))
        l (loop [i 0
                 l s/+black+]
            (if (< i +sample-num+)
              (let [ray (camera/ray camera
                                    (+ x (Math/random))
                                    (+ y (Math/random)))]
                (recur (inc i) (s/+ l (scene/trace scene ray 0))))
              l))
        avg (s/scale l (/ 1 +sample-num+))]
    (apply q/color (s/->color avg))))

(defn setup []
  {:y 0
   :scene (let [scene (scene/->Scene [] [])]
            (-> scene
                (scene/add-obj (g/->Sphere (v/->Vec 0 3 0) 0.5
                                           (g/->Material (s/->Spectrum 0.9 0.9 0.9)
                                                         0 0 1.5 (s/->Spectrum 0.9 0.9 0.9))))
                (scene/add-obj (g/->Sphere (v/->Vec 2.1 0 -2) 0.5
                                           (g/->Material (s/->Spectrum 0.9 0.9 0.9)
                                                         0 0 1.5 (s/->Spectrum 0.9 0.9 0.9))))
                (scene/add-obj (g/->Sphere (v/->Vec -2.1 0 0) 1
                                           (g/->Material (s/->Spectrum 0.9 0.1 0.9)
                                                         0 0 1.5 s/+black+)))
                (scene/add-obj (g/->Sphere (v/->Vec 0 0 0) 1
                                           (g/->Material (s/->Spectrum 0.9 0.9 0.1)
                                                         0.9 0 1.5 s/+black+)))
                (scene/add-obj (g/->Sphere (v/->Vec 2.1 0 0) 1
                                           (g/->Material (s/->Spectrum 0.1 0.9 0.9)
                                                         0 0.8 1.8 s/+black+)))
                (scene/add-obj (g/->CheckedObj (g/make-plane (v/->Vec 0 -1 0) (v/->Vec 0 1 0)
                                                             (g/->Material (s/->Spectrum 0.9 0.9 0.9)
                                                                           0 0 0 s/+black+))
                                               (/ 1 1) (g/->Material (s/->Spectrum 0.2 0.2 0.2)
                                                                     0 0 0 s/+black+)))
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
  :title "trace"
  :setup setup
  :draw draw
  :update update-state
  :key-pressed key-pressed
  :key-released key-released
  :features [:size [640 480]
             :resizable true]
  :middleware [m/fun-mode m/pause-on-error])
