;;; for fun let's make ourselves write this as a series of operations on bytes
;;; so what we do is take in a 32bit number as an input, use that as the automata state,
;;; then use a byte as the rule-set (for the wolfram encoding) and then we create music
;;; extra-credit: if we hit a point where two rounds are the same we end the program


(use-package :bit-smasher)

(defun pad-bits (bs l)
  (if (< (length bs) l)
      (concatenate 'simple-bit-vector (make-array (- l (length bs)) :element-type 'bit) bs)
      bs))

(defun byte-to-rules (r)
  (let ((b (bits<- r)))
  #'(lambda (triple)
      (cond ((equal triple #*111)
	     (bit b 0))
	    ((equal triple #*110)
	     (bit b 1))
	    ((equal triple #*101)
	     (bit b 2))
	    ((equal triple #*100)
	     (bit b 3))
	    ((equal triple #*011)
	     (bit b 4))
	    ((equal triple #*010)
	     (bit b 5))
	    ((equal triple #*001)
	     (bit b 6))
	    ((equal triple #*000)
	     (bit b 7))))))

;;; we want to return the new bit vector at the end of every step
(defun run-sim (a r)
  (let ((result (make-array 32 :element-type 'bit)))
    (dotimes (i 32)
      (cond ((= i 0)
	     (setf (bit result 0) (funcall r (concatenate 'simple-bit-vector #*0 (subseq a 0 2)))))
	    ((= i 31)
	     (setf (bit result 31) (funcall r (concatenate 'simple-bit-vector (subseq a 30 32) #*0))))
	  (t
	   (setf (bit result i) (funcall r (subseq a (- i 1) (+ i 2)))))))
    result))

(defun print-bytes (bs)
  (dotimes (i 4)
    (write-char (code-char (int<- (subseq bs (* 8 i) (+ 8 (* 8 i))))))))


;; here we are going to turn the automata into a function over time
;; to do this we're going to break the 32 bits into a schema like
;; using 2 bit selectors of operations followed by 6 bits of number
;; 00 -> logior 
;; 01 -> *      
;; 10 -> logand
;; 11 -> logxor
;;
;; we choose three of these (24 bits)
;; then leave four bits for a shift (1 bit for left or right) and then
;; 4 bits for a subtraction

(defun to-op (bs)
  (cond ((equal bs #*00) #'logior)
	((equal bs #*01) #'*)
	((equal bs #*10) #'logand)
	((equal bs #*11) #'logxor)))

(defun automata-to-function (bs)
  (let ((o1 (subseq bs 0 2))
	(o2 (subseq bs 8 10))
	(o3 (subseq bs 16 18))
	(s1 (bit bs 18))
	(c1 (int<- (subseq bs 2 8)))
	(c2 (int<- (subseq bs 10 16)))
	(c3 (int<- (subseq bs 18 24)))
	(c4 (int<- (subseq bs 24 28)))
	(c5 (int<- (subseq bs 28 32))))
    #'(lambda (tim) (ldb (byte 8 0)
			 (- (funcall (to-op o1) tim
				     (ash (+ c1
					     (funcall (to-op o2) tim c2)
					     (funcall (to-op o3) tim c3)) c4)) c5)))))
(defun main (args)
  (let ((board (pad-bits (bits<- (parse-integer (nth 1 args))) 32))
	(rules (byte-to-rules (parse-integer (nth 2 args)))))
    (block outer
      (loop
	do (let ((new-board (run-sim board rules))
	        (f (automata-to-function board)))
	     (dotimes (tim 2000)
	       (write-byte (funcall f tim) *standard-output*))
	     ;;(format t "~a~%" board)
	     (if (equal board new-board)
		 (return-from outer)
		 (setf board new-board))))))) 
	     
