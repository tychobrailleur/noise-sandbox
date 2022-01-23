(ns noise-sandbox.core
  (:import [java.awt image.BufferedImage Color]
           [javax.imageio ImageIO]
           [java.io File]))


(defn dot [v1 v2]
  (+ (* (nth v1 0)
        (nth v2 0))
     (* (nth v1 1)
        (nth v2 1))))

(defn floor [ff]
  (let [xi (int ff)]
    (if (< ff xi)
      (dec xi)
      xi)))

(defn constant-vector [v]
  (let [h (bit-and v 3)] ;; == % 4
    (case h
      0 [1.0 1.0]
      1 [-1.0 1.0]
      2 [-1.0 -1.0]
      [1.0 -1.0])))

(defn fade [t]
  ;; uses Perlin's blending function f(t) = 6t^5 - 15t^4 + 10t^3
  (* (+ (* (- (* 6 t) 15) t) 10) t t t))

(defn linear-interp [t a1 a2]
  ;; used to interpolate values
  (+ a1 (* t (- a2 a1))))

(defn permute [n]
  (let [p (vec (shuffle (range n)))]
    (concat p p)))

(def ppp (permute 256))

;; Port of https://rtouti.github.io/graphics/perlin-noise-algorithm

(defn perlin-noise2d [x y]
  (let [xx (bit-and (floor x) 255)
        yy (bit-and (floor y) 255)
        xf (- x (floor x))
        yf (- y (floor y))

        top-right [(- xf 1.0) (- yf 1.0)]
        top-left [xf (- yf 1.0)]
        bottom-right [(- xf 1.0) yf]
        bottom-left [xf yf]

        ;; assign a random value for each corner, while making sure
        ;; a corner always get the same value.
        value-top-right (nth ppp (+ (nth ppp (+ (inc xx))) yy 1))
        value-top-left (nth ppp (+ (nth ppp xx) yy 1))
        value-bottom-right (nth ppp (+ (nth ppp (inc xx)) yy))
        value-bottom-left (nth ppp (+ (nth ppp xx) yy))

        dot-top-right (dot top-right (constant-vector value-top-right))
        dot-top-left (dot top-left (constant-vector value-top-left))
        dot-bottom-right (dot bottom-right (constant-vector value-bottom-right))
        dot-bottom-left (dot bottom-left (constant-vector value-bottom-left))

        u (fade xf)
        v (fade yf)]
    (linear-interp u
                   (linear-interp v dot-bottom-left dot-top-left)
                   (linear-interp v dot-bottom-right dot-top-right))))


;; Calculation of noise with octaves
;; cf. https://www.redblobgames.com/maps/terrain-from-noise/
(defn perlin-noise2d-octaves [n x y]
  (loop [iter n
         freq 1
         res 0
         sum 0]
    (if (= 0 iter)
      (do
        (println sum)
        (/ res sum))
      (recur (dec iter)
             (/ freq 2.0)
             (+ res (* freq (perlin-noise2d (* (* 2 (- (inc n) iter)) x)
                                            (* (* 2 (- (inc n) iter)) y))))
             (+ sum freq)))))


(defn random-range [min max]
  (+ (rand (- max min)) min))

(defn random-noise2d [i j]
  (random-range -1.0 1.0))


;; (defn set-colours [size noise-fn]
;;   (for [j (range size)]
;;     (for [i (range size)]
;;       (let [c (-> (noise-fn (* i 0.01) (* j 0.01))
;;                   (+ 1.0)
;;                   (/ 2.0)
;;                   (* 255)
;;                   (Math/round))]
;;         (java.awt.Color. c c c)))))

(defn set-colours [size noise-fn]
  (for [j (range size)]
    (for [i (range size)]
      (let [c (noise-fn (* i 0.01) (* j 0.01))]
        (cond
          (< c -0.05) (Color. 65 105 225)
          (< c 0) (Color. 238 214 175)
          :else (Color. 34 139 34))))))


(defn draw-image [size noise-fn]
  (let [img (BufferedImage. size size BufferedImage/TYPE_INT_ARGB)
        gfx (.getGraphics img)
        colours (set-colours size noise-fn)]
    (.setColor gfx Color/WHITE)
    (.fillRect gfx 0 0 size size)
    (doseq [j (range size)]
      (doseq [i (range size)]
        (let [colour (-> colours
                         (nth i)
                         (nth j))]
          (.setColor gfx colour)
          (.drawLine gfx i j i j))))
    (ImageIO/write img "png" (File. "/tmp/results.png"))))


(defn -main [& args]
  (draw-image 500 random-noise2d)
  (draw-image 500 perlin-noise2d)
  (draw-image 500 (partial perlin-noise2d-octaves 8)))
