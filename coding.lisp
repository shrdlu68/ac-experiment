(defpackage :arithmetic-coding
  (:use :cl))

(in-package :arithmetic-coding)

(ql:quickload :cl-mathstats)

(declaim (optimize (compilation-speed 0)))

(defparameter *float.5* (coerce 1/2 'short-float))
(defparameter *float.1* (coerce 1/99 'short-float))
(defparameter *float.99* (coerce 99/100 'short-float))

(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector ()
  '(simple-array octet *))

(deftype uint62v ()
  '(simple-array uint62 *))

(deftype uint64 ()
  '(unsigned-byte 64))

(deftype uint62 ()
  '(unsigned-byte 62))

(deftype uint32 ()
  '(unsigned-byte 32))

(load "uint8-bv.lisp")

(defun percentage (m n)
  (* (float (/ m n)) 100))

(defun make-octet-vector (length)
  (make-array length :element-type 'octet))

(defun read-octets (path &optional n skip)
  (with-open-file (fd path :direction :input :element-type 'octet)
    (when skip
      (file-position fd skip))
    (let* ((len (or n (file-length fd)))
	   (ov (make-octet-vector len)))
      (loop for index from 0 below len do
	(setf (aref ov index) (read-byte fd)))
      ov)))

(defun write-octets (ov path)
  (with-open-file (fd path :direction :output
			   :if-does-not-exist :create :if-exists :supersede
			   :element-type 'octet)
    (loop for uint8 across ov do
      (write-byte uint8 fd))))

(declaim (inline bytes-in-int))
(defun bytes-in-int(int)
  "Return the least number of octets needed to represent an integer"
  (ceiling (integer-length int) 8))

(defun octets-to-integer (ov &key (start 0) (end (length ov)))
  (declare (optimize (speed 3)))
  (do ((i (1- end) (1- i))
       (n 0 (+ n 8))
       (int 0 (logior int (ash (aref ov i) n))))
      ((< i start) int)))

(defun integer-to-octets(int &optional (length (bytes-in-int int)))
  "Convert an integer into a network-byte-ordered vector of octets,
   padded with zeros if the number of octets in int is less than length."
  (let ((vec (make-array length :element-type 'octet)))
    (loop
      for pos from 0 by 8
      for index from (1- (length vec)) downto 0
      do
	 (setf (aref vec index)
	       (ldb (byte 8 pos) int)))
    vec))

(defun upscale-P (P f)
  (if (< P 0.5)
      (* P f)
      (- 1 (* (- 1 P) f))))

(defun downscale-P (P factor)
  (let* ((Q (- 1 P))
	 (gap (abs (- P Q)))
	 (mid (+ (min P Q) (* gap 0.5)))
	 (scaled-gap (* gap factor 0.5)))
    (if (< P Q)
	(- mid scaled-gap)
	(+ mid scaled-gap))))

;; (declaim (inline update-db))
(defun update-db (substring string markov-db &key end)
  (declare (optimize (speed 3) (safety 0) (space 0))
	   (type simple-vector markov-db)
	   (type fixnum end))
  (let* ((db-entry (access substring markov-db))
	 (bit (uint62v-bit string end)))
    (declare (type (or null (simple-array short-float (2)))
		   db-entry))
    (cond (db-entry
	   (when (> (aref db-entry 1) (* (- (third substring) (second substring)) 5.0))
	     (setf (aref db-entry 0) 1.0)
	     (setf (aref db-entry 1) 2.0))
	   (when (zerop bit)
	     (incf (aref db-entry 0) 1.0))
	   (incf (aref db-entry 1) 1.0)
	   (setf (access substring markov-db) db-entry))
	  (t
	   (setf (access substring markov-db)
		 (if (zerop bit)
		     (make-array 2 :element-type 'short-float :initial-contents '(1.0 1.0))
		     (make-array 2 :element-type 'short-float :initial-contents '(0.0 1.0))))))))

(defun smoothed-mean (mix)
  (cl-mathstats:mean (loop for n  = mix then (butlast n 1)
		   while n
		   collecting (cl-mathstats:mean n))))

;; (declaim (inline predict))
(defun predict (string markov-db &key (start 0) (end (length string)))
  (declare (optimize (speed 3) (safety 0) (space 0))
	   (type simple-vector markov-db)
	   (type fixnum start end)
	   (type uint62v string))
  (when (< end 1) (return-from predict *float.5*))
  ;; Update the DB
  (loop repeat 30
  	with update-end fixnum = (1- end)
  	with multiple = (- update-end (mod update-end 8))
  	for n fixnum downfrom (if (= multiple update-end)
  				  (- multiple 8) multiple) above 0 by 8
  	while (>= n start)
  	for substring = (list string n update-end)
  	do (update-db substring string markov-db :end update-end))
  ;; get stats
  (let ((mm-stats
  	  (loop with multiple = (- end (mod end 8))
		for n fixnum downfrom (if (= multiple end)
					  (- multiple 8) multiple) above 0 by 8
		while (>= n start)
		with mix = nil
  		while (let* ((substring (list string n end))
  			     (db-entry (access substring markov-db))
  			     (stats (and db-entry
  					 (/ (aref db-entry 0) (aref db-entry 1)))))
			(declare (type (or null
					   (simple-array short-float (2)))
				       db-entry))
  		        (and stats
			     (push stats mix)))
  		finally (return (and mix
				     (cond ((< end 128)
					    nil)
					   ((< (length mix) 3)
					    (downscale-P (cl-mathstats:mean mix) 0.9))
					   (t
					    (smoothed-mean mix))))))))
    (cond ((null mm-stats) *float.5*)
	  (t
	   (cond ((zerop (the short-float mm-stats)) *float.1*)
		 ((= (the short-float mm-stats) 1) *float.99*)
		 (t mm-stats))))))

(defun int-to-bv (int len)
  (let ((bv (make-array len :element-type 'bit)))
    (loop for index from 0 below len
	  for bv-index downfrom (1- (length bv)) do
	    (setf (aref bv bv-index) (ldb (byte 1 index) int)))
    bv))

(declaim (inline bv-to-int))
(defun bv-to-int (bv &key (start 0) (end (length bv)))
  (declare (type simple-bit-vector bv)
	   (type fixnum start end))
  (loop for index fixnum from start below end
	for bit bit = (sbit bv index)
	for int = bit then (+ (* int 2) bit)
	finally (return int)))

(declaim (inline bv-to-uint64))
(defun bv-to-uint64 (bv &key (start 0) (end (length bv)))
  (declare (type simple-bit-vector bv)
	   (type fixnum start end))
  (loop for index fixnum from start below end
	for bit of-type bit = (sbit bv index)
	for int of-type uint64 = bit then (+ (* int 2) bit)
	finally (return int)))

(defun ov-to-bv (ov)
  (declare (type octet-vector ov))
  (let ((bv (the simple-bit-vector
		 (make-array (* (length ov) 8) :element-type 'bit))))
    (loop for uint8 across ov
	  for a from 0 by 8 below (length bv)
	  do (replace bv (int-to-bv uint8 8) :start1 a))
    bv))

(sb-ext:define-hash-table-test uint62bv= sxhash-bits)

(defun encode-arithmetic (ov &key (predict #'predict) (start 0) (end (* 8 (length ov))))
  "The encoder: requires only the bit vector to be encoded as an argument"
  (declare (optimize (speed 3) (safety 0) (space 0))
	   (type fixnum start end)
	   (type function predict))
  (let* ((len (the fixnum (- end start)))
	 (uint62v (ov-to-uint62v ov))
	 (output (make-array 1000000 :element-type 'uint62))
	 (output-index 0))
    (declare (type uint62v uint62v output)
	     (type uint62 output-index))
    (loop with markov-db of-type simple-vector = (make-array (expt 2 25) :element-type
							     'list :initial-element nil)
	  with low of-type uint64 = 0
	  with high of-type uint64 = (1- (expt 2 64))
	  with half of-type uint64 = (* (expt 2 64) 1/2)
	  with qtr of-type uint64 = (* (expt 2 64) 1/4)
	  with qtr3 of-type uint64 = (* (expt 2 64) 3/4)
	  with follow-on of-type fixnum = 0
	  for bit-index fixnum from start below end
	  for bit of-type bit = (uint62v-bit uint62v bit-index)
	  for range of-type uint64 = (- high low)
	  for off-probability of-type short-float = (funcall predict uint62v markov-db :end bit-index)
	  for interval of-type uint64 = (+ low (floor (* range off-probability)))
	  do (if (zerop bit)
		 (setf high interval)
		 (setf low (1+ interval)))
	     (loop while (or (< high half)
			     (<= half low)
			     (and (<= qtr low)
				  (< high qtr3)))
		   do (cond ((< high half)
			     (setf (uint62v-bit output (post-incf output-index)) 0)
			     (when (plusp follow-on)
			       (loop repeat follow-on do
				 (setf (uint62v-bit output (post-incf output-index)) 1))
			       (setf follow-on 0))
			     (setf low (ash (ldb (byte 63 0) low) 1))
			     (setf high (1+ (ash (ldb (byte 63 0) high) 1))))
			    ((<= half low)
			     (setf (uint62v-bit output (post-incf output-index)) 1)
			     (when (plusp follow-on)
			       (loop repeat follow-on do
				 (setf (uint62v-bit output (post-incf output-index)) 0))
			       (setf follow-on 0))
			     (setf low (ash (ldb (byte 63 0) low) 1))
			     (setf high (1+ (ash (ldb (byte 63 0) high) 1))))
			    (t
			     (incf follow-on)
			     (setf low (ash (ldb (byte 63 0) (- low qtr)) 1))
			     (setf high (1+ (ash (ldb (byte 63 0) (- high qtr)) 1))))))
	  finally (setf (uint62v-bit output (post-incf output-index)) 1))
    (setf output (adjust-array output (ceiling output-index 62)))
    (values output len)))

(defun decode-arithmetic (uint62v len &optional (predict #'predict))
  "The decoder: requires the output of the encoder and the length of the output"
  (declare (optimize (speed 3) (safety 0) (space 0))
	   (type uint62v uint62v)
	   (type uint62 len)
	   (type function predict))
  (let* ((output (make-array (ceiling len 62) :element-type 'uint62 :initial-element 0))
	 (bv-len (* 62 (length uint62v)))
	 (final-interval (ash (aref uint62v 0) 2))
	 (bv-pos 64))
    (declare (type uint64 final-interval bv-pos)
	     (type uint62v output))
    (setf (ldb (byte 2 0) final-interval)
	  (ldb (byte 2 60) (aref uint62v 1)))
    (loop with markov-db of-type simple-vector = (make-array (expt 2 25) :element-type
							      'list :initial-element nil)
	  with low of-type uint64 = 0
	  with high of-type uint64 = (1- (expt 2 64))
	  with half of-type uint64 = (* (expt 2 64) 1/2)
	  with qtr of-type uint64 = (* (expt 2 64) 1/4)
	  with qtr3 of-type uint64 = (* (expt 2 64) 3/4)
	  for range of-type uint64 = (- high low)
	  for output-index of-type uint62 from 0 below len
	  for off-probability of-type short-float = (funcall predict output
							     markov-db :end output-index)
	  for interval of-type uint64 = (+ low (floor (* range off-probability)))
	  do (cond ((<= final-interval interval)
		    (setf (uint62v-bit output output-index) 0)
		    (setf high interval))
		   ((<= interval final-interval)
		    (setf (uint62v-bit output output-index) 1)
		    (setf low (1+ interval))))
	     (loop while (or (< high half)
			     (<= half low)
			     (and (<= qtr low)
				  (< high qtr3)))
		   do (cond ((or (< high half)
				 (<= half low))
			     (setf low (ash (ldb (byte 63 0) low) 1))
			     (setf high (1+ (ash (ldb (byte 63 0) high) 1)))
			     (setf range (- high low))
			     (setf final-interval
				   (logior (ash (ldb (byte 63 0) final-interval) 1)
					   (if (< bv-pos bv-len)
					       (uint62v-bit uint62v bv-pos)
					       0))))
			    (t
			     (setf low (ash (ldb (byte 63 0) (- low qtr)) 1))
			     (setf high (1+ (ash (ldb (byte 63 0) (- high qtr)) 1)))
			     (setf range (- high low))
			     (setf final-interval
				   (logior (ash (ldb (byte 63 0) (- final-interval qtr)) 1)
					   (if (< bv-pos bv-len)
					       (uint62v-bit uint62v bv-pos)
					       0)))))
		      (incf bv-pos))
	  finally (setf output (adjust-array output (ceiling output-index 62))))
    output))

(defun test-random (len iterations)
  "Tests the encoder & decoder using len octets from /dev/urandom iteration times"
  (assert (>= len 64))
  (percentage (loop repeat iterations
		    for ov = (read-octets "/dev/urandom" len)
		    for bv = (ov-to-bv ov)
		    counting (multiple-value-bind (out len) (encode-arithmetic bv)
			       (equal (decode-arithmetic out len) bv)))
	      iterations))

(defun test-file (path)
  "Test compression on file at path. Read it all to memory!"
  (let* ((ov (read-octets path))
	 (in-len (length ov))
	 (compressed (encode-arithmetic ov))
	 (out-len (floor (* 62 (length compressed)) 8))
	 (decompressed (decode-arithmetic compressed (* in-len 8)))
	 (decompressed-ov (make-array in-len :element-type 'octet)))
    (loop for index from 0 below (* in-len 8)
	  do (setf (uint8v-bit decompressed-ov index)
		   (uint62v-bit decompressed index)))
    (if (null (mismatch ov decompressed-ov))
	(format t "Test successful~%~
                   Input size: ~Ab~%~
                   Output size: ~Ab~%~
                   Compression ratio: ~A%~%"
		in-len out-len
		(round (* (/ (- in-len out-len) in-len) 100)))
	(format t "Test failed at ~A of ~A~%" (mismatch ov decompressed) in-len))))
