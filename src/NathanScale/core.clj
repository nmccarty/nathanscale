(ns NathanScale.core)

(def PI 3.141592653589793)

(def STANDARD-A 2)

(defn big-l
  ([x]
    (big-l x STANDARD-A))
  ;; Lanczos reconstrucion kernel
  ([x a]
    (if (< (* -1 a) x a)
      (if (= x 0) 
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
          (println r)
          (reduce +
                  (map #(* (print-return (big-l (- x %))) (nth vec %))
                       (range bottom top))))))))

(defn clamp
  "Clamps n to the range defined by lower and upper"
  ^double [^double lower ^double upper ^double n]
  (max lower (min n upper)))


(defn read-raw-image
  "Given a string with the location of a file, try to load it as an image"
  [file-loc]
  (try
    (javax.imageio.ImageIO/read (java.io.File. file-loc))
    (catch Exception e (.getMessage e))))


(defn fancy-clamp
  ^double [num]
  (if (java.lang.Double/isNaN num)
    255
    (clamp 0 255 num)))


(defn floats-to-pixel
  ^long [^double red-f ^double green-f ^double blue-f]
  (let [red   (int red-f)
        green (int green-f)
        blue  (int blue-f)]
    (bit-or red
            (bit-or (bit-shift-left green 8)
                    (bit-shift-left blue 16)))))

(defn write-image-to
  [img loc format]
  (javax.imageio.ImageIO/write img format (java.io.File. loc)))

  

    