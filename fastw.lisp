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

(defun euclid-distance-old (v1 v2)
  (let ((diff (numcl:- v1 v2)))
    (numcl:sum (numcl:* diff diff))))

(defun euclid-distance (v1 v2)
  (let ((diff (mgl-mat:M- v1 v2)))
    (mgl-mat:.*! diff diff)
    (mgl-mat:asum diff)))


(defun find-closest-vector(v1 vectors)
  (let ((best-diff sb-kernel:most-positive-exactly-single-float-fixnum) (best-word nil) (i 0))
    (loop for k being each hash-key of vectors
	  do (let* ((v2 (gethash k vectors))
		    (diff (euclid-distance v1 v2)))
	       (incf i)
	       (when (and (> diff 0) (< diff best-diff))
		 ; (print (format nil "~a - ~a" diff k))
		 (setq best-diff diff)
		 (setq best-word k))))
    best-word
    ()))


(defun find-closest-vector2(v1 vectors)
  (let ((best-diff sb-kernel:most-positive-exactly-single-float-fixnum) (best-word nil) (i 0))
    (loop for k being each hash-key of vectors
	  do (let* ((v2 (gethash k vectors))
		    (diff (- 1 (mgl-mat:dot v1 v2))))
	       (incf i)
	       (when (and (> diff 0) (< diff best-diff))
		 ; (print (format nil "~a - ~a" diff k))
		 (setq best-diff diff)
		 (setq best-word k))))
    best-word
    ()))

(defun restore-vector(filename)
  "Read vectors into global"
  (setf *fast-vectors* (cl-store:restore filename))
  (loop for k being each hash-key of *fast-vectors*
	do (let* ((numbers (gethash k *fast-vectors*))
		  (dim (list 1 (length numbers)))
		  (mnumbers (mgl-mat:make-mat dim :ctype :float :initial-contents (list numbers))))
	     (setf (gethash k *fast-vectors*) mnumbers))))
  
(defun testme()
  (let ((x1 (gethash "kake" *fast-vectors*)))
    (mgl-mat:m+ x1 x1)))

(defun find-closest-word (word hash)
  (let ((w (gethash word hash)))
    (find-closest-vector w hash)))

(defun word-vectors(sentence hash)
  (let* ((words (remove-duplicates (str:words (sb-unicode:lowercase sentence)) :test #'equal))
	 (vectors (loop for w in words when (gethash w hash) collect (list w (gethash w hash)))))
    vectors))

(defun word-vectors-from-words(words hash)
   (loop for w in words collect (list w (gethash w hash))))

(declaim (inline div-vector))
(defun div-vector(v d &optional (size 300))
  "Divides vector by d ration, helper"
  (let ((div-vec (mgl-mat:make-mat (list 1 size) :ctype :float :initial-element (/ 1 d))))
    (mgl-mat:.*! v div-vec)
    div-vec))

(defun sum-vectors(vs)
  "Sums a list of vectors, helper"
   (reduce #'(lambda (a b) (mgl-mat:m+ a b)) vs))

(defun average-document-vector(sentence hash &optional (size 300))
  "Find average document vector, based on each term"
  (let* ((vectors (word-vectors sentence hash))
	 (vectors-only (mapcar #'second vectors))
	 (vector-sum (sum-vectors vectors-only)))
    (div-vector vector-sum (length vectors-only) size)))

(defun tf-document-vector(sentence hash tf-large tf-small &optional (weight 1) (tf-weight 1) (size 300))
  (let* ((words-all (str:words (str:remove-punctuation (sb-unicode:lowercase sentence))))
	 (words (loop for w in words-all when (gethash w hash) collect w))
	 (words-no-dup (remove-duplicates words :test #'equal)) 
	 (vectors (word-vectors-from-words words-no-dup hash))
	 (vectors-only (mapcar #'second vectors))
	 (tf-idfs (combined-tf-idfs words-all words-no-dup tf-large tf-small weight))
	 (tf-vectors (loop for tf-idf in tf-idfs
			   for v in vectors-only
			   for w in words-no-dup
			   do (print (format nil " ~a - ~a - ~a" w tf-idf (expt (+ 1 tf-idf) tf-weight)))
			   collect (let ((mul-vec (mgl-mat:make-mat (list 1 size) :ctype :float :initial-element (expt (+ 1 tf-idf) tf-weight))))
				     (mgl-mat:.*! v mul-vec)
				     mul-vec))))
    ; (print (format nil "Length: ~a - ~a: " (length words-no-dup) (length vectors)))
    (div-vector (sum-vectors tf-vectors) (length tf-idfs) size)))



(defun find-closest-n(v1 vectors n)
  "Use like this:  (find-closest-n (tf-document-vector 'denne handler om javascript jeg bor i bergen på vestlandet' *fast-vectors* large-tf small-tf 0.2 2) *fast-vectors* 10 )"
  (let ((i 0)
	(heap (make-instance 'cl-heap:binary-heap :sort-fun #'(lambda (a b) (> (cdr a) (cdr b)))))
	)
    (loop for k being each hash-key of vectors
	  do (let* ((v2 (gethash k vectors))
		    (diff (euclid-distance v1 v2)))
	       (incf i)
	       (when (or (<= (cl-heap:heap-size heap) n) (<= diff (cl-heap:peep-at-heap heap)))
		 (cl-heap:add-to-heap heap (cons k diff))
		 (when (>= (cl-heap:heap-size heap) n)
		   (cl-heap:pop-heap heap)))))
    (loop while (> (cl-heap:heap-size heap) 0)
	  do (print (format nil "~a" (cl-heap:pop-heap heap))))))

