;;; 
;;; A jackdaw implementation of the "enculturation model" described in chapter 
;;; five of Van der Weij (2020) and used in chapter seven of Van der Weij
;;; (2020).
;;;

(in-package #:jackdaw)

(defmethod meter-domain ((m enculturation))
  (loop for m in (meter-parameters m) collect (car (car m))))

(defmodel enculturation ()
  (training? ioi-domain meter-parameters) 
  ((M ; meter
      (^m)
      (categorical
       () :parameters (meter-parameters enculturation))
      (persistent (meter-domain model))
      :key (lambda (m) (list (getf m 'period) (getf m 'pulses))))
   (D ; downbeat distance
      (^d ^p m)
      (accumulator-model (m)
			 :training? (training? enculturation))
      (recursive (loop for ioi in (ioi-domain model)
		       collect (cons (+ $^p ioi) $^d))
		 (deterministic '(*)))
      :formatter (lambda (d) (car d)))
   (P ; phase
      (^p m d)
      (uniform ())
      (recursive (list (mod (car $d) (car $m)))
		 (loop for phase below (car $m)
		       collect phase)))
   (IOI (d ^p ^ioi)
	(uniform ())
	(if (eq (car $d) '*) '(*)
	    (list (- (car $d) $^p)))
	:hidden nil)))
