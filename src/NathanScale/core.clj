(ns NathanScale.core)

(def PI 3.141592653589793)

(def STANDARD-A 2.0)



(defn peek!
  "A peek that works on transients, because for some reason clojure doesn't give me this"
  [seq]
  (nth seq (dec (count seq))))


(defn big-l
  ([^double x]
    (big-l x STANDARD-A))
  ;; Lanczos reconstrucion kernel
  ([^double x ^double a]
    (if (< (* -1 a) x a)
      (if (= x (double 0)) 
        1
        (/ (* a (Math/sin (* PI x)) (Math/sin (/ (* PI x) a))) (* PI PI) (* x x)))
      0)))

(defn print-return
  [val]
  (do
    (println val)
    val))

(defn interpolation-function
  "Constructs an interpolation function for a given vector"
  ([vec]
    (interpolation-function vec STANDARD-A))
  ([vec a]
    (let [len (count vec)]
      (fn [x]
        (let [top    (int (min len (inc (+ a (Math/floor x)))))
              bottom   (int (max 0 (- (+ (Math/floor x) 1) a)))
              r                              (range bottom top)]
          (reduce +
                  (map #(* (big-l (- x %)) (nth vec %))
                       (range bottom top))))))))

(defn resize-to
  "Make a new vector that is vec resized to newsize"
  [vec newsize]
  (let [size                    (count vec)
        ratio              (/ newsize size)
        interp (interpolation-function vec)
        newvec (transient [])]
    (doseq [i (range 0 newsize)]
      (let [oldloc (/ i ratio)]
       ;; (conj! newvec (fancy-clamp (interp oldloc)))))
        (conj! newvec (interp oldloc))))
    (persistent! newvec)))

(defn clamp
  "Clamps n to the range defined by lower and upper"
  ^double [^double lower ^double upper ^double n]
  (max lower (min n upper)))

(defn fancy-clamp
  [num]
  (if (java.lang.Double/isNaN num)
    255
    (clamp 0 255 num)))

(defn floats-to-pixel
  [red-f green-f blue-f]
  (let [red   (int (fancy-clamp red-f))
        green (int (fancy-clamp green-f))
        blue  (int (fancy-clamp blue-f))]
    (bit-or red
            (bit-or (bit-shift-left green 8)
                    (bit-shift-left blue 16)))))

(defn read-image
  "Given a string with the location of a file, try to load it as an image"
  [file-loc]
  (try
    (javax.imageio.ImageIO/read (java.io.File. file-loc))
    (catch Exception e (.getMessage e))))

(defn image-to-vectors
  "Given an image split it into 3 vectors, one for each channel"
  [img]
  (let [height    (.getHeight img)
        width      (.getWidth img)
        red-vec     (transient [])
        green-vec   (transient [])
        blue-vec    (transient [])]
    (doseq [y (range 0 height)]
      ;; Change to vector-of short?
      (let [red-row   (transient [])
            green-row (transient [])
            blue-row  (transient [])]
        (doseq [x (range 0 width)]
          (let [pixel (.getRGB img x y)
                red   (bit-and pixel 0xFF)
                green (bit-and (bit-shift-right pixel 8) 0xFF)
                blue  (bit-and (bit-shift-right pixel 16) 0xFF)]
            (conj! red-row red)
            (conj! green-row green)
            (conj! blue-row blue)))
        (conj! red-vec (persistent! red-row))
        (conj! green-vec (persistent! green-row))
        (conj! blue-vec (persistent! blue-row))))
    (hash-map :red (persistent! red-vec)
              :green (persistent! green-vec)
              :blue (persistent! blue-vec)
              :height height
              :width width)))

(defn resize-width-to
  "Given a map of channel vectors, create a new map that is the resized image with the given width"
  [img-vecs new-width]
  (let [old-red-vec (:red img-vecs)
        old-green-vec (:green img-vecs)
        old-blue-vec  (:blue img-vecs)
        height        (:height img-vecs)]
    (hash-map :red (pmap #(resize-to % new-width) old-red-vec)
              :green (pmap #(resize-to % new-width) old-green-vec)
              :blue (pmap #(resize-to % new-width) old-blue-vec)
              :height height
              :width new-width)))

(defn resize-height-to
  "Given a map of channel vectors, create a new map that is the resized image with the given height"
  [img-vecs new-height]
  ;; Doing one channel at a time, this is so I can add concurrency later
  (let [width         (:width img-vecs)
        old-red       (:red img-vecs)
        new-red-vec   (let [new-red (transient [])]
                                (doseq [y (range 0 new-height)]
                                  (conj! new-red (transient [])))
                                (doseq [x (range 0 width)]
                                  (let [old-col (map #(nth % x) old-red)
                                        new-col (resize-to old-col new-height)]
                                    (doseq [y (range 0 new-height)]
                                      (conj! (nth new-red y) (nth new-col y)))))
                        (map persistent! (persistent! new-red)))
        old-green     (:green img-vecs)
        new-green-vec (let [new-green (transient [])]
                                 (doseq [y (range 0 new-height)]
                                   (conj! new-green (transient [])))
                                 (doseq [x (range 0 width)]
                                   (let [old-col (map #(nth % x) old-green)
                                         new-col (resize-to old-col new-height)]
                                     (doseq [y (range 0 new-height)]
                                       (conj! (nth new-green y) (nth new-col y)))))
                         (map persistent! (persistent! new-green)))
        old-blue      (:blue img-vecs)
        new-blue-vec  (let [new-blue (transient [])]
                                (doseq [y (range 0 new-height)]
                                  (conj! new-blue (transient [])))
                                (doseq [x (range 0 width)]
                                  (let [old-col (map #(nth % x) old-blue)
                                        new-col (resize-to old-col new-height)]
                                    (doseq [y (range 0 new-height)]
                                      (conj! (nth new-blue y) (nth new-col y)))))
                        (map persistent! (persistent! new-blue)))]
    
    ;; Return that ulgy sonofabitch 
    (hash-map :red new-red-vec
              :green new-green-vec
              :blue new-blue-vec
              :height new-height
              :width width)))

(defn img-vectors-to-img
  [img-vecs]
  (let [width     (:width img-vecs)
        height    (:height img-vecs)
        img       (java.awt.image.BufferedImage. width height java.awt.image.BufferedImage/TYPE_INT_RGB)
        red-vec   (:red img-vecs)
        green-vec (:green img-vecs)
        blue-vec  (:blue img-vecs)]
    (doseq [y (range 0 height)
            x (range 0 width)]
      (let [r     (nth (nth red-vec y) x)
            g     (nth (nth green-vec y) x)
            b     (nth (nth blue-vec y) x)
            pixel (floats-to-pixel r g b)]
        (.setRGB img x y pixel)))
    img))

(defn write-image-to
  [img loc format]
  (javax.imageio.ImageIO/write img format (java.io.File. loc)))

(defn test-me
  []
  (write-image-to (img-vectors-to-img
                    (resize-height-to 
                      (resize-width-to
                        (image-to-vectors
                          (read-image "C:/Users/natman3400/cirno.png"))
                        200)
                      200))
                  "C:/Users/natman3400/test.png"
                  "png"))

  

    