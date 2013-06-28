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