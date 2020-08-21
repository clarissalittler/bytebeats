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
  (let ((result (make-array 64 :element-type 'bit)))
    (dotimes (i 64)
      (cond ((= i 0)
	     (setf (bit result 0) (funcall r (concatenate 'simple-bit-vector #*0 (subseq a 0 2)))))
	    ((= i 63)
	     (setf (bit result 63) (funcall r (concatenate 'simple-bit-vector (subseq a 62 64) #*0))))
	  (t
	   (setf (bit result i) (funcall r (subseq a (- i 1) (+ i 2)))))))
    result))

(defun print-bytes (bs)
  (dotimes (i 8)
    (write-char (code-char (int<- (subseq bs (* 8 i) (+ 8 (* 8 i))))))))


(defun main (args)
  (let ((board (pad-bits (bits<- (parse-integer (nth 1 args))) 64))
	(rules (byte-to-rules (parse-integer (nth 2 args)))))
    (block outer
      (loop
	do (let ((new-board (run-sim board rules)))
	     (print-bytes board)
	     ;;(format t "~a~%" board)
	     (if (equal board new-board)
		 (return-from outer)
		 (setf board new-board))))))) 
	     
