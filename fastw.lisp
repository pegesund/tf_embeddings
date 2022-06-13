;;;; fastw.lisp


(in-package #:fastw)

(defvar fast-vector-file "/home/petter/Nedlastinger/cc.no.300.vec")

(defvar *fast-vectors* nil)
(defparameter *max-vectors* 500000)

(defun handle-fline (line)
  (let* ((all (str:split " " line))
	 (word (string-downcase (car all)))
	 (vec-string (cdr all))
	 (vec (mapcar #'parse-number:parse-number vec-string))
	 (numarr (numcl:asarray vec))
	 (l2-norm (numcl:sqrt (numcl:sum (numcl:* numarr numarr))))
	 (norm-arr
	   (if (> l2-norm 0)
	       (numcl:/ numarr l2-norm)
	       numarr))
	 )
    ; (print word)
    (setf (gethash word  *fast-vectors*) norm-arr)))

(defun get-file ()
  "Get fasttext vectors into hashmap. L2-nomralize vectors on the way in, so we can use eucledian measures"
  (setf cl-progress-bar:*progress-bar-enabled* t)
  (setq *fast-vectors* (make-hash-table :test 'equal :synchronized T))
  (cl-progress-bar:with-progress-bar (*max-vectors* "Transforming  ~a vectors" *max-vectors*)
    (let ((i 0)
	  (x (open fast-vector-file)))
      (when x
	(loop for line = (read-line x nil)
	      while (and line (< i *max-vectors*))
	      do (progn (when (> i 1) (handle-fline line))
			(cl-progress-bar:update 1)
			(incf i)
			)))
      (close x)))
  (print "Now storing vectors in file")
  (cl-store:store *fast-vectors* "/var/tmp/hypp.txt")
  (print "Done storing")
  )

(defun euclid-distance (v1 v2)
  (let ((diff (numcl:- v1 v2)))
    (numcl:sum (numcl:* diff diff))))

(defun find-closest-word(v1 vectors)
  (let ((best-diff 2) (best-word nil) (i 0))
    (loop for k being each hash-key of vectors
	  do (let* ((v2 (gethash k vectors))
		    (diff (euclid-distance v1 v2)))
	       (incf i)
	       (when (= 0 (mod i 10000))
		 (print i))
	       (when (and (> diff 0) (< diff best-diff))
		 (print (format nil "~a - ~a" diff k))
		 (setq best-diff diff)
		 (setq best-word k))))
    best-word
    ()))


(defun restore-vector(filename)
  (setf *fast-vectors* (cl-store:restore filename))
  (loop for k being each hash-key of *fast-vectors*
	do (let* ((numbers (gethash k *fast-vectors*))
		  (dim (list 1 (length numbers)))
		  (mnumbers (mgl-mat:make-mat dim :ctype :float :initial-contents (list numbers))))
	     (setf (gethash k *fast-vectors*) mnumbers))))
  
