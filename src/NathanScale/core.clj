(ns NathanScale.core)

(def PI 3.141592653589793)

(defn sinc 
  "Normalized sinc function"
  [x]
  (if (= 0 x)
    1
    (/ (Math/sin (* PI x)) (* PI x))))

(defn sinc-2d
  "Extension of the sinc function to two dimensions"
  [x y]
  (* (sinc x) (sinc y)))

(defn clamp
  "Clamps n to the range defined by lower and upper"
  [lower upper n]
  (max lower (min n upper)))

(defn sinc-point-builder
  "Creates a sinc function with a peak of value at (x,y)"
  [x y value]
  (fn [z w]
    (* value (sinc-2d (- z x) (- w y)))))

(defn read-raw-image
  [file-loc]
  (try
    (javax.imageio.ImageIO/read (java.io.File. file-loc))
    (catch Exception e (.getMessage e))))

(defn read-image-from-file-at
  [file-loc]
  (let [img    (read-raw-image file-loc)
        vec    (atom [])
        height (.getHeight img)
        width  (.getWidth img)]
    (doseq [x (range 0 width)
            y (range 0 height)]
      (let [pixel  (.getRGB img x y)
            redval (bit-and pixel 0xff)
            greval (bit-and (bit-shift-right pixel 8) 0xff)
            bluval (bit-and (bit-shift-right pixel 16) 0xff)
            mappy  (hash-map :red (sinc-point-builder x y redval)
                             :green (sinc-point-builder x y greval)
                             :blue (sinc-point-builder x y bluval))]
        (swap! vec #(conj % mappy))))
    (hash-map :height    height
              :width     width
              :functions @vec)))
    