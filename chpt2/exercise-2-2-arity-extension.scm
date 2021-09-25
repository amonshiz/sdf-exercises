(define (compose3 f g)
  (define (the-composition . args)
    (assert (procedure-arity-valid? g (length args)))
    (let ((res (apply g args)))
      (if (list? res)
	  (
	    (assert (procedure-arity-valid? f (length res)))
	    (apply f res)
	  )
	  (
	    (assert (or (= 1 (procedure-arity-min (procedure-arity f)))
			(= 1 (procedure-arity-max (procedure-arity f)))))
	    (f res)
	  )
      )))
  (make-generic-procedure (max (procedure-arity-min (procedure-arity f)) (procedure-arity-min (procedure-arity g)))))

(define (spread-combine2 h f g)
  (let ((f-arity (procedure-arity f)) (g-arity (procedure-arity g)) (h-arity (procedure-arity h)))
    (assert (not (eqv? #f (procedure-arity-max f-arity)))) ; f has to be finite min and max
    (let ((tmin (+ (procedure-arity-min f-arity) (procedure-arity-min g-arity)))
	  (tmax (and (procedure-arity-max f-arity) (procedure-arity-max g-arity) (+ (procedure-arity-max f-arity) (procedure-arity-max-g-arity)))))
      (define (the-combination . args)
	(assert (or (not tmax) (<= (length args) tmax)))
	(let ((max-n 
