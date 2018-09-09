(defpackage :arithmetic-coding
  (:use :cl))

(in-package :arithmetic-coding)

(ql:quickload :cl-mathstats)

(rename-package :cl-mathstats :math)

(defparameter *float.5* (coerce 1/2 'short-float))
(defparameter *float.1* (coerce 1/99 'short-float))
(defparameter *float.99* (coerce 99/100 'short-float))

(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector ()
  '(simple-array octet *))

(deftype uint64 ()
  '(unsigned-byte 64))

(deftype uint32 ()
  '(unsigned-byte 32))

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

(declaim (inline pos-stats))
(defun pos-stats (pos string &key (start 0))
  (declare (type simple-bit-vector string)
	   (type fixnum pos start))
  (loop with counts of-type short-float = 0.0
	with p0 of-type short-float = 0.0
	for n fixnum from 8 to 8
	when (zerop (mod pos n))
	  do (loop for index fixnum from start by n below pos
		   with total of-type short-float = 0.0
		   with count0 of-type short-float = 0.0
		   do (when (zerop (sbit string index))
			(incf count0))
		      (incf total)
		   finally (and (plusp total)
				(let* ((P (/ count0 total)))
				  (when (or (> P 0.9)
					    (< P 0.1))
				    (incf p0 P)
				    (incf counts)))))
	finally (return (and (plusp counts)
			     (/ p0 counts)))))

(declaim (inline update-db))
(defun update-db (substring string markov-db &key end)
  (declare (type simple-bit-vector string substring)
	   (type hash-table markov-db)
	   (type fixnum end))
  (let* ((db-entry (gethash substring markov-db))
	 (bit (sbit string end))
	 (len (length substring)))
    (declare (type (or null (simple-array short-float (2)))
		   db-entry))
    (cond (db-entry
	   (when (> (aref db-entry 1) (* len 5.0))
	     (setf (aref db-entry 0) 1.0)
	     (setf (aref db-entry 1) 2.0))
	   (when (zerop bit)
	     (incf (aref db-entry 0) 1.0))
	   (incf (aref db-entry 1) 1.0)
	   (setf (gethash substring markov-db) db-entry))
	  (t
	   (setf (gethash substring markov-db)
		 (if (zerop bit)
		     (make-array 2 :element-type 'short-float :initial-contents '(1.0 1.0))
		     (make-array 2 :element-type 'short-float :initial-contents '(0.0 1.0))))))))

(defun smoothed-mean (mix)
  (math:mean (loop for n  = mix then (butlast n (floor (length mix) 1.6))
		   while n
		   collecting (math:mean n))))

(declaim (inline predict))
(defun predict (string markov-db &key (start 0) (end (length string)))
  (declare (optimize (speed 3))
	   (type simple-bit-vector string)
	   (type hash-table markov-db)
	   (type fixnum start end))
  (when (< end 1) (return-from predict *float.5*))
  ;; Update the DB
  (loop repeat 30
	with update-end fixnum = (1- end)
	with multiple = (- update-end (mod update-end 8))
	for n fixnum downfrom (if (= multiple update-end)
				  (- multiple 8) multiple) by 8
	while (>= n start)
	for substring of-type bit-vector = (subseq string n update-end)
	do (update-db substring string markov-db :end update-end))
  ;; Get stats
  (let ((pos-stats (pos-stats end string))
  	(mm-stats
  	  (loop with multiple = (- end (mod end 8))
		for n fixnum downfrom (if (= multiple end)
					  (- multiple 8) multiple) by 8
		while (>= n start)
		with mix = nil
  		while (let* ((substring (subseq string n end))
  			     (db-entry (gethash substring markov-db))
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
					    (downscale-P (math:mean mix) 0.9))
					   ((> (length mix) 3)
					    (smoothed-mean mix))
					   (t
					    (math:mean mix))))))))
    (cond ((and pos-stats
		(or (zerop pos-stats)
		    (= pos-stats 1)))
	   (setf mm-stats pos-stats))
	  ((and mm-stats pos-stats)
	   (setf mm-stats (/ (the short-float (+ pos-stats (the short-float mm-stats))) 2))))
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

(defun encode-arithmetic (bv &key (predict #'predict) (start 0) (end (length bv)))
  "The encoder: requires only the bit vector to be encoded as an argument"
  (declare (type simple-bit-vector bv)
	   (type fixnum start end))
  (let* ((len (the fixnum (- end start)))
	 (output (make-array len :element-type 'bit :adjustable t :fill-pointer 0)))
    (loop with markov-db of-type hash-table = (make-hash-table :test #'equal)
	  with low of-type uint64 = 0
	  with high of-type uint64 = (1- (expt 2 64))
	  with half of-type uint64 = (* (expt 2 64) 1/2)
	  with qtr of-type uint64 = (* (expt 2 64) 1/4)
	  with qtr3 of-type uint64 = (* (expt 2 64) 3/4)
	  with follow-on of-type fixnum = 0
	  for bit-index fixnum from start below end
	  for bit of-type bit = (sbit bv bit-index)
	  for range of-type uint64 = (- high low)
	  for off-probability of-type short-float = (funcall predict bv markov-db :end bit-index)
	  for interval of-type uint64 = (+ low (floor (* range off-probability)))
	  do (if (zerop bit)
		 (setf high interval)
		 (setf low (1+ interval)))
	     (loop while (or (< high half)
			     (<= half low)
			     (and (<= qtr low)
				  (< high qtr3)))
		   do (cond ((< high half)
			     (vector-push-extend 0 output)
			     (when (plusp follow-on)
			       (loop repeat follow-on do (vector-push-extend 1 output))
			       (setf follow-on 0))
			     (setf low (ash (ldb (byte 63 0) low) 1))
			     (setf high (1+ (ash (ldb (byte 63 0) high) 1))))
			    ((<= half low)
			     (vector-push-extend 1 output)
			     (when (plusp follow-on)
			       (loop repeat follow-on do (vector-push-extend 0 output))
			       (setf follow-on 0))
			     (setf low (ash (ldb (byte 63 0) low) 1))
			     (setf high (1+ (ash (ldb (byte 63 0) high) 1))))
			    (t
			     (incf follow-on)
			     (setf low (ash (ldb (byte 63 0) (- low qtr)) 1))
			     (setf high (1+ (ash (ldb (byte 63 0) (- high qtr)) 1))))))
	  finally (vector-push-extend 1 output))
    (values (subseq output 0 (fill-pointer output)) len)))

(defun decode-arithmetic (bv len &optional (predict #'predict))
  "The decoder: requires the output of the encoder and the length of the output"
  (let* ((output (make-array len :element-type 'bit))
	 (bv-len (length bv))
	 (final-interval (bv-to-uint64 bv :start 0 :end 64))
	 (bv-pos 64))
    (declare (type uint64 final-interval bv-pos)
	     (type simple-bit-vector output))
    (loop with markov-db of-type hash-table = (make-hash-table :test #'equal)
	  with low of-type uint64 = 0
	  with high of-type uint64 = (1- (expt 2 64))
	  with half of-type uint64 = (* (expt 2 64) 1/2)
	  with qtr of-type uint64 = (* (expt 2 64) 1/4)
	  with qtr3 of-type uint64 = (* (expt 2 64) 3/4)
	  for range of-type uint64 = (- high low)
	  for output-index from 0 below len
	  for off-probability of-type short-float = (funcall predict output markov-db :end output-index)
	  for interval of-type uint64 = (+ low (floor (* range off-probability)))
	  do (cond ((<= final-interval interval)
		    (setf (sbit output output-index) 0)
		    (setf high interval))
		   ((<= interval final-interval)
		    (setf (sbit output output-index) 1)
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
					       (sbit bv bv-pos)
					       0))))
			    (t
			     (setf low (ash (ldb (byte 63 0) (- low qtr)) 1))
			     (setf high (1+ (ash (ldb (byte 63 0) (- high qtr)) 1)))
			     (setf range (- high low))
			     (setf final-interval
				   (logior (ash (ldb (byte 63 0) (- final-interval qtr)) 1)
					   (if (< bv-pos bv-len)
					       (sbit bv bv-pos)
					       0)))))
		      (incf bv-pos)))
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
	 (bv (ov-to-bv ov))
	 (in-len (length ov) )
	 (compressed (encode-arithmetic bv))
	 (out-len (round (/ (length compressed) 8)))
	 (decompressed (decode-arithmetic compressed (* in-len 8))))
    (if (equal decompressed bv)
	(format t "Test successful~%~
                   Input size: ~Ab~%~
                   Output size: ~Ab~%~
                   Compression ratio: ~A%~%"
		in-len out-len
		(round (* (/ (- in-len out-len) in-len) 100)))
	(format t "Test failed at ~A of ~A~%" (mismatch bv decompressed) (length bv)))))
