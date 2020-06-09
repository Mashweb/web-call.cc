(load "browser-util.scm")
(define (test)
  (let loop
      ((()))
    (display "Pick a tool.")
    (with-handlers ((click-handler "#move"))
      (let* ((junk (get-input))
	     (op "move"))
	(display "You have chosen the Move tool.")
	(let loop
	    ((()))
	  ;;(display "loop")
	  (with-handlers ((dragstart-handler "#body") ;; Change the selector after testing.
			  (dragover-handler "#body")
			  (drop-handler "#body"))
	    (let* ((input (get-input))
		   (event-type (js-ref (first input) "name"))
		   (event (second input)))
	      (case event-type
		(("dragstart")
		 (display "dragstart"))
		(("dragover")
		 ;;(console-log "dragover")
		 ;;(console-dir event)
		 (js-invoke event "preventDefault")
		 (js-set! event "dataTransfer.dropEffect" "move"))
		(("drop")
		 (display "drop")
		 (js-invoke event "preventDefault")
		 (perform-operation op event))
		(else
		 (display "unknown")))))
	  (loop))))
    (loop)))

(define (perform-operation op ev)
  (case op
    (("move")
     (console-dir ev)
     (console-dir (getelem1 "#div2"))
     (js-invoke (js-ref ev "target") "appendChild" (getelem1 "#div2")))
    (else
     (display "Unknown operation"))))
