(ns NathanScale.core)

(def PI 3.141592653589793)

(defn sinc 
  "Normalized sinc function"
  ^double [^double x]
  (if (= 0 x)
    1
    (/ (Math/sin (* PI x)) (* PI x))))

(defn sinc-2d
  "Extension of the sinc function to two dimensions"
  ^double [^double x ^double y]
  (* (sinc x) (sinc y)))

(defn clamp
  "Clamps n to the range defined by lower and upper"
  ^double [^double lower ^double upper ^double n]
  (max lower (min n upper)))

(defn sinc-point-builder
  "Creates a sinc function with a peak of value at (x,y)"
  [x y value]
  (fn 
    ^double [^double z ^double w]
    (* value (sinc-2d (- z x) (- w y)))))

(defn read-raw-image
  "Given a string with the location of a file, try to load it as an image"
  [file-loc]
  (try
    (javax.imageio.ImageIO/read (java.io.File. file-loc))
    (catch Exception e (.getMessage e))))

(defn read-image-from-file-at
  "Given location of file, load as image and construct functions"
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
                             :blue (sinc-point-builder x y bluval)
                             :x-pos x
                             :y-pos y)]
        (swap! vec #(conj % mappy))))
    (hash-map :height    height
              :width     width
              :functions @vec
              :image     img)))

(defn fancy-clamp
  ^double [num]
  (if (java.lang.Double/isNaN num)
    255
    (clamp 0 255 num)))

(defn diffrence-ratio
  [a b]
  (/ (java.lang.Math/abs (- a b)) (max a b)))

(defn within-x-ratio
  [x a b]
  (if (= 0 (Math/abs (- a b)))
    true
  (<= (diffrence-ratio a b) x)))

(defn within-w-of
  [w x y]
  (<= (Math/abs (- x y)) w))

(defn build-single-function-from-image
  [img-map]
  (let[img-vec (:functions img-map)
       height  (:height img-map)
       width   (:width img-map)]
    (hash-map :func (fn [^double x ^double y ^long channel]
                      (fancy-clamp
                        (cond
                          (= 0 channel) (let [funs (pmap :red (filter #(and (within-w-of 10 x (:x-pos %))
                                                                           (within-w-of 10 y (:y-pos %)))
                                                                     img-vec))]
                                          (reduce +
                                                  (pmap #(% x y) funs)))
                          (= 1 channel) (let [funs (pmap :green (filter #(and (within-w-of 10 x (:x-pos %))
                                                                             (within-w-of 10 y (:y-pos %)))
                                                                     img-vec))]
                                          (reduce +
                                                  (pmap #(% x y) funs)))
                          :else         (let [funs (pmap :blue (filter #(and (within-w-of 10 x (:x-pos %))
                                                                            (within-w-of 10 y (:y-pos %)))
                                                                     img-vec))]
                                          (reduce +
                                                  (pmap #(% x y) funs))))))
              :height height
              :width  width)))

(defn floats-to-pixel
  ^long [^double red-f ^double green-f ^double blue-f]
  (let [red   (int red-f)
        green (int green-f)
        blue  (int blue-f)]
    (bit-or red
            (bit-or (bit-shift-left green 8)
                    (bit-shift-left blue 16)))))

(defn build-image-of-size-with
  [^long width ^long height fun-map]
  (let [func         (:func fun-map)
        old-width    (:width fun-map)
        width-ratio  (/ width old-width)
        old-height   (:height fun-map)
        height-ratio (/ height old-height)
        ^java.awt.image.BufferedImage img (java.awt.image.BufferedImage. width height java.awt.image.BufferedImage/TYPE_INT_RGB)]
    (doseq [x (range 0 width)
            y (range 0 height)]
      (let[old-x (/ x width-ratio)
           old-y (/ y height-ratio)
           pix   (int (floats-to-pixel (func old-x old-y 0)
                                       (func old-x old-y 1)
                                       (func old-x old-y 2)))]
        (.setRGB img x y pix)))
    img))

(defn write-image-to
  [img loc format]
  (javax.imageio.ImageIO/write img format (java.io.File. loc)))

(defn test-me []
  (write-image-to (build-image-of-size-with 100 100
                                         (build-single-function-from-image (read-image-from-file-at "C:/Users/natman3400/cirno.png")))
                  "C:/Users/natman3400/test-new.png"
                  "png"))
  

    