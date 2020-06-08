(load "mini-framework.scm")
(define (test)
  (let loop
      ((()))
    (with-handlers ((dragstart-handler "#menu") ;; Change the selector after testing.
		    (dragover-handler "#body")
		    (drop-handler "#body"))
		   (let ((event (symbol->string (car (get-input)))))
		     (case event
		       (("dragstart")
			(display "dragstart"))
		       (("dragover")
			(display "dragover"))
		       (("drop")
			(display "drop"))
		       (else
			(display "unknown")))))
    (loop))
		    
  (display "Test finished."))
