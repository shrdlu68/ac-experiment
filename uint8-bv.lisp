(declaim (inline uint8v-bit))
(defun uint8v-bit (ov n)
  (multiple-value-bind (uint8 bit) (truncate n 8)
    (setf bit (abs (+ bit -7)))
    (ldb (byte 1 bit) (aref ov uint8))))

(declaim (inline (setf foo)))
(defun (setf uint8v-bit) (bit ov n)
  (multiple-value-bind (uint8 bit-pos) (truncate n 8)
    (setf bit-pos (abs (+ bit-pos -7)))
    (unless (array-in-bounds-p ov uint8)
      (setf ov (adjust-array ov (+ (length ov) (truncate (length ov) 3)) :initial-element 0)))
    (setf (ldb (byte 1 bit-pos) (aref ov uint8)) bit)))

(declaim (ftype (function (uint62v uint62) bit))
	 (inline uint62v-bit))
(defun uint62v-bit (uint62v n)
  (declare (optimize (speed 3) (safety 0) (space 0))
	   (type uint62v uint62v)
	   (type uint62 n))
  (multiple-value-bind (uint62 pos) (truncate n 62)
    (declare (uint62 uint62 pos))
    (setf pos (abs (+ pos -61)))
    (ldb (byte 1 pos) (aref uint62v uint62))))

(declaim (ftype (function (bit uint62v uint62) bit))
	 (inline (setf uint62v-bit)))
(defun (setf uint62v-bit) (bit uint62v n)
  (declare (optimize (speed 3) (safety 0) (space 0))
	   (type uint62v uint62v)
	   (type uint62 n)
	   (type bit bit))
  (multiple-value-bind (uint62 pos) (truncate n 62)
    (declare (uint62 uint62 pos))
    (setf pos (abs (+ pos -61)))
    (setf (ldb (byte 1 pos) (aref uint62v uint62)) bit)))

(declaim (inline ov-to-uint62v))
(defun ov-to-uint62v (ov)
  (let* ((len (* (length ov) 8))
	 (out-len (ceiling len 62))
	 (out (make-array out-len :element-type 'uint62 :initial-element 0)))
    (loop for index from 0 below len
	  do (setf (uint62v-bit out index)
		   (uint8v-bit ov index)))
    out))

(defun print62 (n)
  (format t "~62,'0B~%" n) (force-output))

(declaim (ftype (function (uint62v uint62 uint62) uint62))
	 #|(inline uint62v-ldb)|#)
(defun uint62v-ldb (v start end)
  (declare (optimize (speed 3) (safety 0) (space 0))
	   (type uint62 start end)
	   (type uint62v v))
  (multiple-value-bind (start-byte mask-start) (truncate start 62)
    (declare (type uint62 start-byte)
	     (type (integer 0 61) mask-start))
    (let* ((len (min 62 (- end start)))
	   (end-byte (truncate (the uint62 (1- end)) 62))
	   (res 0))
      (declare (type uint62 res end-byte)
	       (type (integer 1 62) len))
      (cond ((= start-byte end-byte)
	     (setf res (mask-field (byte len (- 62 (+ len mask-start))) (aref v start-byte)))
	     (setf res (ash res mask-start)))
	    (t
	     (let* ((first-len (- 62 mask-start))
		    (second-len (- len first-len))
		    (tmp 0))
	       (declare (type (integer 1 62) first-len second-len)
			(type uint62 tmp))
	       (setf res (mask-field (byte first-len 0) (aref v start-byte)))
	       (setf res (ash res (- 62 first-len)))
	       (setf tmp (ldb (byte second-len (- 62 second-len)) (aref v (1+ start-byte))))
	       (setf tmp (ash tmp (- 62 len)))
	       (setf res (logior res tmp)))))
      res)))

(defun print-uint62v(obj)
  (destructuring-bind (v start end) obj
    (with-output-to-string (out)
      (loop for start-pos from start by 62 below end
	    for end-pos = (min end (+ start-pos 62))
	    do (format out "~62,'0B" (uint62v-ldb v start-pos end-pos))))))

;; (defun sxhash-bits (obj)
;;   (destructuring-bind (v start end) obj
;;     (declare (type fixnum start end)
;; 	     (type uint62v v))
;;     (let* ((len (- end start))
;; 	   (uint62 (loop with x of-type uint62 = 0
;; 			 for s fixnum from start by 62 below end
;; 			 for e fixnum = (min end (+ s 62))
;; 			 for y of-type uint62 = (uint62v-ldb v s e)
;; 			 do (setf x (logxor x y))
;; 			 finally (return x))))
;;       (declare (type uint62 uint62)
;; 	       (type uint62 len))
;;       (setf len (mod (* len 4609481938760227381)
;; 		     (expt 2 62)))
;;       (setf uint62 (mod (* len 4609481938760227381)
;; 		     (expt 2 62)))
;;       ;; (setf len (sb-rotate-byte:rotate-byte (logcount uint62) (byte 62 0) len))
;;       (logxor len uint62))))

(declaim (ftype (function (uint62 uint62) uint62))
	 (inline mul))
(defun mul (a b)
  (declare (optimize (speed 3) (safety 0) (space 0))
	   (type uint62 a b))
  (mod (* a b) (expt 2 62)))

(declaim (ftype (function (list) uint62))
	 #|(inline sxhash-bits)|#)
(defun sxhash-bits (obj)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (destructuring-bind (v start end) obj
    (declare (type fixnum start end)
	     (type uint62v v))
    (let* ((len (- end start))
	   (uint62 (loop for s fixnum from start by 62 below end
			 for e fixnum = (min end (+ s 62))
			 for y of-type uint62 = (uint62v-ldb v s e)
			 for x of-type uint62 = (mul 4571440376359939919 y) then (logxor x y)
			 finally (return x))))
      (declare (type uint62 uint62 len))
      (setf len (mul 4609481938760227381 len))
      (setf len (logxor len 3074457345618258602))
      (setf uint62 (logxor len uint62))
      (loop for n of-type uint62 across #(4609481938760227381 4608667863816187813
      					  4580132245254002491 4505749082374480937
      					  4435394148475023187 4426118641117530787)
	    for o of-type uint62 = (ash uint62 -10)
      	    do
	       (setf uint62 (logxor uint62 (mul o n))))
      uint62)))

;; (setf len (sb-rotate-byte:rotate-byte (logcount uint62) (byte 62 0) len))
;; 62-bit prime numbers
;; 4426118641117530787
;; 4435394148475023187
;; 4447022137236106471
;; 4461973493974365199
;; 4505749082374480937
;; 4571440376359939919
;; 4580132245254002491
;; 4590000399353973311
;; 4601100010485772777
;; 4608667863816187813
;; 4609481938760227381
;; 4610679650187512389
;; 4610922117718114339

;; 3074457345618258602 #b10101010...62

(declaim (ftype (function (list) boolean))
	 #|(inline uint62bv=)|#)
(defun uint62bv= (a b)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (destructuring-bind (v start-a end-a) a
    (declare (optimize (speed 3) (safety 0))
	     (ignorable v)
	     (type fixnum start-a end-a))
    (destructuring-bind (v start-b end-b) b
      (declare (type fixnum start-b end-b)
	       (type uint62v v))
      (let ((len-a (- end-a start-a))
	    (len-b (- end-b start-b)))
	(and (= len-a len-b)
	     (loop for start-pos-a of-type fixnum from start-a by 62 below end-a
		   for end-pos-a of-type fixnum = (min end-a (+ start-pos-a 62))
		   for start-pos-b of-type fixnum from start-b by 62 below end-b
		   for end-pos-b of-type fixnum = (min end-b (+ start-pos-b 62))
		   for x of-type uint62 = (uint62v-ldb v start-pos-a end-pos-a)
		   for y of-type uint62 = (uint62v-ldb v start-pos-b end-pos-b)
		   always (= x y)))))))

(defmacro post-incf (var) `(prog1 ,var (incf ,var)))

;; (declaim (inline access))
(defun access (key db)
  (declare (optimize (speed 3) (safety 0) (space 0))
	   (type simple-vector db))
  (let* ((hash (sxhash-bits key))
	 (index (logand (1- (expt 2 25)) hash))
	 (keys (aref db index))
	 (val (find key keys :key (lambda (x) (declare (type simple-vector x)) (aref x 0))
			     :test #'uint62bv=)))
    (declare (type uint62 hash index)
	     (type list keys))
    (and val (aref val 1))))

;; (declaim (inline (setf access)))
(defun (setf access) (value key db)
  (declare (optimize (speed 3) (safety 0) (space 0))
	   (type simple-vector db))
  (let* ((hash (sxhash-bits key))
	 (index (logand (1- (expt 2 25)) hash))
	 (keys (aref db index))
	 (val (find key keys :key (lambda (x) (declare (type simple-vector x)) (aref x 0))
			     :test #'uint62bv=)))
    (declare (type uint62 hash index)
	     (type list keys))
    (if val
	(setf (aref val 1) value)
	(push (vector key value) (aref db index)))))

;; (loop repeat 100000
;;       always (loop with x = (random (expt 2 240))
;; 		   with len = (random 240)
;; 		   with start-a = (random 3000)
;; 		   with start-b = (+ 4000 (random 3000))
;; 		   for index-a from start-a
;; 		   for index-b from start-b
;; 		   for bit-pos from 0 below len
;; 		   for bit = (ldb (byte 1 bit-pos) x)
;; 		   do (setf (uint62v-bit uint62v index-a) bit)
;; 		      (setf (uint62v-bit uint62v index-b) bit)
;; 		   finally
;; 		      (return (uint62bv= (list uint62v start-a (+ start-a len))
;; 					 (list uint62v start-b (+ start-b len))))))
