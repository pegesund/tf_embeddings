;;;; fastw.lisp


(in-package #:fastw)

(defvar fast-vector-file "/home/petter/Nedlastinger/cc.no.300.vec")

(defvar *fast-vectors* nil)
(defvar *job-vectors* nil)
(defvar large-tf nil)
(defvar small-tf nil)
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


; input in form (list word vector score)
; best is on form (list w1 w2 score)

(defun find-closest-pair(elements best)
  (let ((so-far-best best))
    (if (>= (length elements) 2)
	(let* ((e1 (car elements))
	       (w1 (car e1))
	       (v1 (cadr e1))
	       (re (cdr elements)))
	  (loop for e in re
		do (let* ((dist (euclid-distance (cadr e) v1))
			  (score (* (- 2 dist) (expt (+ (caddr e) (caddr e1)) 0.7)))
			  )
		     (when (< 0 score) (print (list (car e) (car e1) score dist)))
		     (when (or (not so-far-best) (>= score (caddr so-far-best)))
		       (setf so-far-best (list w1 (car e) score)))))
	  (find-closest-pair re so-far-best))
      so-far-best)))
	     

(defun tf-document-vector(sentence hash tf-large tf-small &optional (weight 1) (tf-weight-p 0) (size 300) (topn 40))
  (let*  ((words-all (str:words (str:remove-punctuation (sb-unicode:lowercase sentence))))
	  (tf-weight (if (> tf-weight-p 0)
			 tf-weight-p
			 (+ 1 (max 1 (min 4 (* 0.03 (length words-all)))))))
	  (words (loop for w in words-all when (gethash w hash) collect w))
	  (words-no-dup (remove-if-not #'(lambda (x) (lookup tf-large x))   (remove-duplicates words :test #'equal)))
	  (topnn (min topn (length words-no-dup))) 
	  (vectors (word-vectors-from-words words-no-dup hash))
	  (vectors-only (mapcar #'second vectors))
	  (total 0)
	  (tf-idfs (combined-tf-idfs words-all words-no-dup tf-large tf-small weight))
	  (sorted-tf-and-vecs (subseq (sort (mapcar #'list words-no-dup vectors-only tf-idfs) #'>  :key #'caddr) 0 topnn))
	  (best-pair (find-closest-pair sorted-tf-and-vecs nil))
	  (tf-vectors (loop for tf-idf in tf-idfs
			    for v in vectors-only
			    for w in words-no-dup
			    ; do (print (format nil "Weight: ~a ~a" w tf-idf))
			    collect (let* ((scale-factor (if (or
							      (string= w (car best-pair))
							      (string= w (cadr best-pair)))
							     (caddr best-pair)
							     1))							   
					   (vector-scale (expt (+ 1 (* tf-idf scale-factor)) tf-weight))
					   (mul-vec (mgl-mat:make-mat (list 1 size) :ctype :float :initial-element vector-scale)))
				      (print (format nil "~a - ~a " w vector-scale))
				      (incf total vector-scale)
				      (mgl-mat:.*! v mul-vec)
				      mul-vec))))
					; (print (format nil "Weight: ~a" tf-idfs))
					; (print sorted-tf-and-vecs)
    (print "Best pair: ")
    (print best-pair)
    ; (print average-topscore)
    ; (print adjust-score)
    (div-vector (sum-vectors tf-vectors) total size)))



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


(defun runme()
  (setf large-tf (load-tf "/data/lisp/large_tf.txt"))
  (setf small-tf (load-tf "/data/lisp/small_tf.txt"))
  (restore-vector "/data/lisp/vectors.txt")
  )

; makes l2 from blas vector
(defun l2(v size)
  (let* ((l2-sum (mgl-mat:nrm2 v))
	 (l2-vec (div-vector v l2-sum size)))
    l2-vec))

(defun parse-file(f)
  (setq *job-vectors*  (make-hash-table :test 'equal :synchronized T))
  (with-open-file (file f)
    (loop for i from 0
        for line = (read-line file nil nil)
        while line
	  do (let* ((wvector (tf-document-vector line *fast-vectors* large-tf small-tf 0.2 3))
		    
		    (key line))
	       (when (> (length key) 71)
		 (setf (gethash (subseq key 0 70) *job-vectors*) wvector)))))) 			   
  
;;; examples

; (find-closest-n (tf-document-vector "Det finnes ingen standardisert definisjon av begrepet hetebølge som brukes felles av alle land. Det kommer blant annet av at det som oppleves som unormale temperaturer i ett område, kan være normale temperaturer i et annet område. I tillegg er det mange ulike kriterier som kan legges til grunn for å definere en hetebølge. Blant annet valg av minimumslengde på antall dager med varme, hvilken eller hvilke temperaturmålinger som brukes (døgn-gjennomsnitt, døgn-minimum eller døgn-maksimum), og om luftfuktighet også tas med i beregningen. Både opplevd varme og faktiske konsekvenser av høy temperatur vil avhenge av hvor man befinner seg. Det betyr at definisjonen man velger i hvert enkelt land, er fastsatt etter hva som er målet med å formidle hetebølgene i områdene landet dekker." *fast-vectors* large-tf small-tf 0.2 3) *fast-vectors* 10)

; (find-closest-n (tf-document-vector "Jeg er flink i javascript og kan mye om frontend. Jeg er utdannet i Bergen på Vestlandet" *fast-vectors* large-tf small-tf 0.2 3) *fast-vectors* 10)

; (find-closest-n (tf-document-vector "Jeg har jobbet som konditor og er flink til å bake kaker, kjeks og annet bakverk. På grunn av mitt finske opphav snakker jeg også finsk" *fast-vectors* large-tf small-tf 0.2 3) *fast-vectors* 10)

					; (find-closest-n (tf-document-vector "Jeg har drevet med oppdrett av fisk i flere år" *fast-vectors* large-tf small-tf 0.2 3) *job-vectors* 10)

					; (find-closest-n (tf-document-vector "jeg er god på linux og scripting og har jobbet med drift" *fast-vectors* large-tf small-tf 0.2 3) *job-vectors* 10)

;  (find-closest-n (tf-document-vector "jeg har erfaring som daglig leder" *fast-vectors* large-tf small-tf 0.2 3) *job-vectors* 10)


;  (find-closest-n (tf-document-vector "jeg har utdannelse fra oslo i programmering med fokus på backend og algoritmer jeg er flink i java og kan litt net jeg har også vært systemutvikler når jeg gikk i lære" *fast-vectors* large-tf small-tf 0.2) *job-vectors* 10)


					; (find-closest-n (tf-document-vector "olje og gass senior ingeniør elektro norconsult norconsult as er med sine ansatte norges største flerfaglige rådgiver rettet mot samfunnsplanlegging og prosjektering vi leverer tjenester nasjonalt og internasjonalt knyttet til bygg og eiendom energi industri miljø olje og gass samferdsel samfunnsplanlegging sikkerhet og vann og avløp selskapet som er ansatteid har hovedkontor i sandvika og mer enn andre kontorer i norge og i utlandet olje og gass senior ingeniør elektro norconsult søker nye senior ingeniører med bakgrunn fra olje og gassprosjekter vær med på å utforme norconsults olje og gassaktiviteter olje og gass er et definert hovedsatsingsområde innenfor norconsult vi er inne i en strategisk omleggingsfase og søker derfor etter flere dyktige ingeniører med fartstid innenfor olje og gassprosjekter norconsult olje og gass i norconsult er i rask vekst etter stor tilgang på prosjekter både på kontorene i sandvika og i larvik vårt hovedfokus er tidligfasestudier for installasjoner til havs og på land våre oppdragivere er større aktører innen primært oljeselskaper og energiselskaper nasjonalt og internasjonalt olje og gass avdelingene er også ansvarlig for utvikling av termisk energi som gasskraftanlegg i utlandet finansiert av world bank olje og gass miljøet arbeider med alle aspekter av aktiviteter knyttet til tjenester for olje og gassindustrien samt termisk industri våre satsningsområder er tidligfase studier for nye og gamle offshore installasjoner lpg lagring og transport samt å være rådgivende ingeniører i utbyggingsprosjekter felles for alle prosjektene er å være med og bidra til utvikling av miljøvennlige løsninger for olje og gass prosessering samt energiproduksjon vi skal være et konstruktivt bidrag til våre kunder for at de skal nå sine mål arbeidsoppgaver overordnet systemdesign av elkraft forsyningsanlegg i tidligfaseoppdrag med kostnadskalkyler teknisk ledelsesrådgivning og oppfølging av engineering kontraktorer på vegne av våre oppdragsgivere oppdragsledelse disiplinledelse ved detaljprosjektering i større tverrfaglige oppdrag oppfølging av epc kontrakter eller pakkeleveranser på vegne av oppdragsgivere tredjepartsverifikasjoner lede mc og commissioning aktiviteter i oppdrag ønskede kvalifikasjoner sivilingeniør ingeniør med god bakgrunn vil også komme i betraktning minimum års relevant erfaring god fremstillingsevne både på engelsk og norsk gode samarbeidsevner og evne til å dele kunnskap med andre arbeide selvstendig og som del av et team erfaring fra å jobbe som en del av operatørens prosjektteam vi tilbyr gode lønns og ansettelsesbetingelser store faglige utfordringer der du har mulighet til å kunne utvikle deg videre i spennende prosjekter sammen med dyktige medarbeidere godt arbeidsmiljø og godt sosialt miljø mulighet for utstasjonering fokus på kompetanse og personalutvikling kurs og opplæring studieturer ulike sosiale arrangementer idrettslag bedriftshytter mm innsendelse av søknad søknad med cv vitnemål og attester sendes via vårt elektroniske søknadsskjema på våre internettsider vi gjør oppmerksom på at det kun er elektroniske søknader som vil bli behandlet *li-nc send søknad fylke akershus søknadsfrist september tiltredelse etter avtale arbeidssted sandvika send søknad kontaktpersoner harald hesselberg" *fast-vectors* large-tf small-tf 0.2 3) *fast-vectors* 10)

; (find-closest-n (tf-document-vector "marinbiolog" *fast-vectors* large-tf small-tf 0.2 3) *job-vectors* 10)

; (find-closest-n (tf-document-vector "jeg er interessert i økonomi og har jobbet som eiendomsutvikler" *fast-vectors* large-tf small-tf 0.2 3) *job-vectors* 10)
