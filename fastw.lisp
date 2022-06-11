;;;; fastw.lisp

(in-package #:fastw)

(defvar fast-vector-file "/home/petter/Nedlastinger/cc.no.300.vec")

(defparameter *fast-vectors* nil)
(defparameter *max-vectors* 10000)

(defun handle-fline (line)
  (let* ((all (str:split " " line))
	 (word (string-downcase (car all)))
	 (vec-string (cdr all))
	 (vec (mapcar #'parse-number:parse-number vec-string))
	 (numarr (numcl:asarray vec))
	 (l2-norm (numcl:sqrt (numcl:sum (numcl:* numarr numarr))))
	 (norm-arr
	   (if (> l2-norm 0)
	       (numcl:* numarr (numcl:/ 1 l2-norm))
	       numarr))
	 )
    ; (print word)
    (setf (gethash word  *fast-vectors*) norm-arr)))

(defun get-file ()
  "Get fasttext vectors into hashmap. L2-nomralize vectors on the way in, so we can use eucledian measures"
  (setq *fast-vectors* (make-hash-table :test 'equal :synchronized T))
  (let ((i 0)
	(x (open fast-vector-file)))
    (when x
      (loop for line = (read-line x nil)
            while (and line (< i *max-vectors*))
            do (progn (when (> i 1) (handle-fline line))
		      (incf i)
		      (when (= 0 (mod i 1000)) (print i))
		      )))
    (close x))
  (cl-store:store *fast-vectors* "/var/tmp/hypp.txt")
  )
