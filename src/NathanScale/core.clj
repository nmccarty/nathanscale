(ns NathanScale.core
  (:gen-class))

(def PI 3.1415926535897)

(def STANDARD-A 4.0)

(defn peek!
  "A peek that works on transients, because for some reason clojure doesn't give me this"
  [seq]
  (nth seq (dec (count seq))))

(defn big-l
  "Lanczos reconstrucion kernel"
  ([^double x]
    (big-l x STANDARD-A))
  ([^double x ^double a]
    (if (< (* -1 a) x a)
      (if (= x (double 0)) 
        1
        (/ (* a (Math/sin (* PI x)) (Math/sin (/ (* PI x) a))) (* PI PI) (* x x)))
      0)))

(defn interpolation-function
  "Constructs an interpolation function for a given vector"
  ([vec]
    (interpolation-function vec STANDARD-A))
  ([vector ^double a]
    (let [len (count vector)]
      (fn [^double x]
        (let [top    (int (min len (inc (+ a (Math/floor x)))))
              bottom   (int (max 0 (- (+ (Math/floor x) 1) a)))
              r                              (range bottom top)]
          (reduce +
                  (map (fn 
                         [^long %]
                         (* (big-l (- x %)) (nth vector %)))
                       (range bottom top))))))))

(defn resize-to
  "Make a new vector that is vec resized to newsize"
  [vector ^long newsize]
  (let [size                    (int (count vector))
        ratio              (double (/ newsize size))
        interp (interpolation-function vector) ;; Build the function used for interpolation
        newvec (transient [])]
    (doseq [i (range 0 newsize)]
      (let [oldloc (double (/ i ratio))]
        (conj! newvec (int (interp oldloc))))) ;; Testing the int
    (persistent! newvec)))

(defn clamp
  "Clamps n to the range defined by lower and upper"
  ^double [^double lower ^double upper ^double n]
  (max lower (min n upper)))

(defn fancy-clamp
  "A version of clamp with the bounds predefined and protecting against NaN"
  [^double num]
  (if (java.lang.Double/isNaN num)
    255.0
    (clamp 0 255 num)))

(defn floats-to-pixel
  "Given three float values, return the integer repusentation of their pixel"
  [^double red-f ^double green-f ^double blue-f]
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
  [^java.awt.image.BufferedImage img]
  (let [height    (.getHeight img)
        width      (.getWidth img)
        red-vec     (transient [])
        green-vec   (transient [])
        blue-vec    (transient [])]
    (doseq [y (range 0 height)]
      ;; Make places to put the rows
      (let [red-row   (transient [])
            green-row (transient [])
            blue-row  (transient [])]
        ;; Go across the row
        (doseq [x (range 0 width)]
          (let [pixel (.getRGB img x y) ;; Get the pixel
                red   (bit-and pixel 0xFF) ;; Extract red channel
                green (bit-and (bit-shift-right pixel 8) 0xFF) ;; Extract green channel
                blue  (bit-and (bit-shift-right pixel 16) 0xFF)] ;; Extract blue channel
            ;; Put things in their places 
            (conj! red-row red) 
            (conj! green-row green)
            (conj! blue-row blue)))
        ;; Now that we are done making these rows, we can go ahead and make them persistant 
        ;; and add them to the vectors for their respecrtive fields 
        (conj! red-vec (persistent! red-row))
        (conj! green-vec (persistent! green-row))
        (conj! blue-vec (persistent! blue-row))))
    ;; Return a hash-map containing the fields and some other stuff
    (hash-map :red (vec (map #(into (vector-of :double) %) (persistent! red-vec))) ;; Put everything into vectors of doubles to avoid boxing
              :green (vec (map #(into (vector-of :double) %) (persistent! green-vec)))
              :blue (vec (map #(into (vector-of :double) %) (persistent! blue-vec)))
              :height height
              :width width)))

(defn resize-width-to
  "Given a map of channel vectors, create a new map that is the resized image with the given width"
  [img-vecs ^long new-width]
  (let [old-red-vec (:red img-vecs)
        old-green-vec (:green img-vecs)
        old-blue-vec  (:blue img-vecs)
        height        (:height img-vecs)]
    (hash-map :red (vec (pmap #(into (vector-of :double) (resize-to % new-width)) old-red-vec))
              :green (vec (pmap #(into (vector-of :double) (resize-to % new-width)) old-green-vec))
              :blue (vec (pmap #(into (vector-of :double) (resize-to % new-width)) old-blue-vec))
              :height height
              :width new-width)))

(defn- resize-height-helper
  "This Function exists to make multithreading resize-hight-to eaiser.
   Simply resizes a given field to the given new-height."
  [old ^long new-height ^long width]
  (let [new (transient [])]
    (doseq [y (range 0 new-height)]
      (conj! new (transient [])))
    (doseq [x (range 0 width)]
      (let [old-col (into (vector-of :double) (map #(nth % x) old))
            new-col (resize-to old-col new-height)]
        (doseq [y (range 0 new-height)]
          (conj! (nth new y) (nth new-col y)))))
    (let [persist-new (persistent! new)]
      (vec (map persistent! persist-new)))));; Testing the vec

(defn resize-height-to
  "Given a map of channel vectors, create a new map that is the resized image with the given height"
  [img-vecs ^long new-height]
  ;; Doing one channel at a time, this is so I can add concurrency later
  (let [width         (int (:width img-vecs))
        old-red       (:red img-vecs)
        new-red-vec   (agent old-red)
        old-green     (:green img-vecs)
        new-green-vec (agent old-green)
        old-blue      (:blue img-vecs)
        new-blue-vec  (agent old-blue)]
    
    ;; Send the agents off to do their thing and resize dat image
    (send new-red-vec #(resize-height-helper % new-height width))
    (send new-green-vec #(resize-height-helper % new-height width))
    (send new-blue-vec #(resize-height-helper % new-height width)) ;; This one is speical 
    
    ;; Wait for the agents to finish their jobs so we don't break something
    (await new-red-vec)
    (await new-green-vec)
    (await new-blue-vec)
    
    ;; Return that ulgy sonofabitch 
    (hash-map :red (deref new-red-vec)
              :green (deref new-green-vec)
              :blue (deref new-blue-vec)
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
  [x y]
  (write-image-to (img-vectors-to-img
                    (resize-height-to 
                      (resize-width-to
                        (image-to-vectors
                          (read-image "C:/Users/natman3400/cirno.png"))
                        x)
                      y))
                  "C:/Users/natman3400/test.png"
                  "png"))

(defn- read-height
  [curr-height]
  (print (str "Input new height (default " curr-height "):"))
  (flush)
  (let [input (.trim (read-line))]
    (if (<= (.length input) 0)
      curr-height
      (java.lang.Integer/parseInt input))))

(defn- read-width
  [curr-width]
  (print (str "Input new width (default " curr-width "):"))
  (flush)
  (let [input (.trim (read-line))]
    (if (<= (.length input) 0)
      curr-width
      (java.lang.Integer/parseInt input))))

(defn- read-save-to
  []
  (print "Input location to save image to: ")
  (flush)
  (let [save-loc (read-line)
        format   (.substring save-loc (inc (.lastIndexOf save-loc ".")))]
    (hash-map :location save-loc
              :format format)))

(defn -main
  [& args]
  (if (<= (count args) 0)
    (do
      (print "Input location of image to be resized: ")
      (flush)
      (let [file-loc   (read-line)
            orig-img   (read-image file-loc)
            img-vecs   (image-to-vectors orig-img)
            new-height (read-height (:height img-vecs))
            new-width  (read-width (:width img-vecs))]
        (println "Resizing ...")
        (let [new-image (img-vectors-to-img
                          (resize-height-to
                            (resize-width-to
                              img-vecs
                              new-width)
                            new-height))
              save-map  (read-save-to)]
          (write-image-to new-image
                          (:location save-map)
                          (:format save-map))
          (System/exit 0))))))

  

    