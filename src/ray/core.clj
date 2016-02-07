(ns ray.core
  (require [ray.vec :as v]
           [quil.core :as q]
           [quil.middleware :as m]))

(in-ns 'ray.core)

(def v1 (v/->Vec 2 3 4))
(def v2 (v/->Vec 3 4 5))
(def v3 (v/->Vec 1 1 1))


(def eye (v/->Vec 0 0 3))
(def sphere-center (v/->Vec 0 0 0))
(def sphere-radius 1)
(def light-pos (v/->Vec 10 10 10))
(def light-power 3000)
(def +no-hit+ (Float/POSITIVE_INFINITY))


(defn diffuse-lighting [p n light-pos light-power]
  (let [v (v/- light-pos p)
        l (v/normalize v)
        dot (v/dot n l)]
    (if (> dot 0)
      (let [r (v/length v)]
        (/ (* light-power dot)
           (* 4 (Math/PI) r r)))
      0)))

(defn intersect-ray-sphere [ray-origin ray-dir
                            sphere-center sphere-radius]
  (let [v (v/- ray-origin sphere-center)
        b (v/dot ray-dir v)
        c (- (v/dot v v) (* sphere-radius sphere-radius))
        d (- (* b b) c)]
    (if (< d 0)
      +no-hit+
      (let [s (Math/sqrt d)
            t (- (- b) s)
            t (if (<= t 0) (- s b) t)]
        (if (> t 0)
          t
          +no-hit+)))))

(defn calc-pixel-color [x y]
  (let [image-plane (q/height)
        ray-dir (v/normalize
                 (v/->Vec
                  (+ x 0.5 (- (/ (q/width) 2)))
                  (- (+ y 0.5 (- (/ (q/height) 2))))
                  (- image-plane)))
        t (intersect-ray-sphere eye
                              ray-dir
                              sphere-center
                              sphere-radius)]
    (if (= t +no-hit+)
      (q/color 0)
      (let [p (v/+ eye (v/scale ray-dir t))
            n (v/normalize (v/- p sphere-center))
            brightness (diffuse-lighting p n
                                         light-pos light-power)
            i (int (min (* brightness 255) 255))]
        (q/color i)))))

(defn setup []
  {:y 0})

(defn update-state [state]
  (assoc state :y (inc (:y state))))

(defn draw [{:keys [y] :as state}]
  (doseq [x (range (q/width))]
    (let [c (calc-pixel-color x y)]
      (q/set-pixel x y c)))
  (if (>= y (q/height))
    (q/no-loop)))

(defn key-pressed [state event]
  (q/start-loop)
  (assoc state :y 0))

(defn key-released [state]
  state)

(defn start []
  (q/defsketch ray
    :title "symphony"
    :setup setup
    :draw draw
    :update update-state
    :key-pressed key-pressed
    :key-released key-released
    :features [:size [640 480]
               :resizable true]
    :middleware [m/fun-mode m/pause-on-error]))
