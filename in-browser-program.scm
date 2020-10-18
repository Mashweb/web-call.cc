;; Copyright (C) 2020 Thomas Elam
;;
;; This file is part of web-call.cc.
;;
;; web-call.cc is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; web-call.cc is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with web-call.cc.  If not, see <https://www.gnu.org/licenses/>.

(load "browser-util.scm")

(define-macro (str-rm-last-char! str)
  `(begin
     (set! ,str (substring ,str 0 (- (string-length ,str) 1)))
     ,str))

;; FIXME: The logic in this function is rather complex. Check it.
;;
;; We will not try to support fully standard Scheme datum syntax, but here is a reference to the symbol syntax:
;;
;; The formal grammar of Scheme variables as defined in TSPL4:
;; <symbol>      --> <initial> <subsequent>*
;; <initial>     --> <letter> | ! | $ | % | & | * | / | : | < | = | > | ? | ~ | _ | ^
;;                            | <Unicode Lu, Ll, Lt, Lm, Lo, Mn, Nl, No, Pd, Pc, Po, Sc, Sm, Sk, So, or Co>
;;                            | \x <hex scalar value>
;; <subsequent>  --> <initial> | <digit 10> | . | + | - | @ | <Unicode Nd, Mc, or Me>
;; <letter>      --> a | b | ... | z | A | B | ... | Z
(define (get-word whence prompt)
  (let ((textarea (element-new '(textarea id "word" rows "1" cols "42"))))
    (element-append-child! whence textarea)
    (sleep 0.1) ;; FIXME
    (js-set! textarea "value" prompt)
    (js-invoke textarea "focus")
    (js-set! textarea "selectionEnd" 0)
    (with-handlers ((keyup-handler "#word"))
      (let ((char #f)
	    (finished #f)
	    (aborted #f)
	    (str #f)
	    (real-num #f)
	    (first-char #t)
	    (got-sym #f))

	;; Get and examine the first character
	
	(set! char (get-char))
	(console-log (format #f "char => ~a" char))
	(set! str (extract-text textarea))
	
	(case char
	  ((43 189 46 48 49 50 51 52 53 54 55 56 57) ; First character is +, -, ., or digit.

	   ;; Getting number

	   (begin
	     ;; Upon getting the first key, clear the prompt.
	     (clear-prompt! textarea)
	     (set! str (extract-text textarea))
	     (while (not finished)
	       (begin
		 (set! char (get-char))
		 (set! str (extract-text textarea))
		 
		 (case char
		   ((8)     ; FIXME: This is just to test to see if a BACKSPACE can be input.
		    (console-log "BACKSPACE"))
		   
		   ((13 32) ; RETURN, SPACE. Considered terminator characters.
		    (set! finished #t)
		    (str-rm-last-char! str))

		   ((27)    ; ESCAPE. Considered an abort character.
		    (set! finihed #t)
		    (set! aborted #t)
		    (set-rm-last-char! str))
		   
		   ((46)    ; Period
		    (if real-num
			(str-rm-last-char! str)          ; Remove extraneous '.'.
			(set! real-num #t)))
		   
		   ((43 45) ; +, -
		    (if first-char
			(set! signed #t)
			(str-rm-last-char! str)))        ; Remove extraneous sign.
		   
		   ((48 49 50 51 52 53 54 55 56 57) ;; Digit.
		    (console-log "digit"))

		   (else
		    (str-rm-last-char! str)))
		 (set! first-char #f)))))        ; Remove extraneous character.

	  ;; Testing for symbol
	  
	  (else
	   (case char
	     ((33 36 37 38 42 47 58 60 61 62 63 126 95 94)  ; !, $, %, &, *, /, :, <, =, >, ?, ~, _, ^
	      (console-log "symbol name initial character, not letter")
	      (set! got-sym #t))

	     (else
	      (if (letter? char)
		  (set! got-sym #t)
		  (begin
		    (console-log "initial character not of number or symbol")
		    (clear-prompt! textarea)
		    (set! str "")))))

	   (if (and got-sym (not finished))

	       ;; Getting symbol

	       (begin
		 
		 ;; Upon getting the first key, clear the prompt.
		 (clear-prompt! textarea)
		 (set! str (extract-text textarea))
		 (while (not finished)
		   (begin
		     (set! char (get-char))
		     (set! str (extract-text textarea))

		     (case char
		       ((48 49 50 51 52 53 54 55 56 57)  ; Digit
			(console-log "digit"))

		       ((13 32) ; RETURN, SPACE. Considered terminator characters.
			(set! finished #t)
			(str-rm-last-char! str))

		       ((27)    ; ESCAPE. Considered a cancel character.
			(set! finihed #t)
			(set! aborted #t)
			(set-rm-last-char! str))
		   
		       (else
			(if (letter? char)
			    (console-log "letter")
			    (str-rm-last-char! str))))
		     (set! first-char #f)))))))

	(element-remove! textarea)
	str))))

(define (letter? char)
  (or
   (and (> char 64) (< char 91))   ;; [A-Z]
   (and (> char 96) (< char 123)))) ;; [a-z]
	    
(define (get-char)
  (event->charcode (second (get-input))))

(define (event->charcode jq-event)
  (js-ref jq-event (if (js-undefined? (js-ref jq-event "which"))
		       "keyCode" ; For IE8
		       "which")))

(define (extract-text textarea)
  (js-ref textarea "value"))
  
(define (clear-prompt! textarea)
  (js-set! textarea "value" (substring (js-ref textarea "value") 0 1)))

(define (test)
  (define jq-event #f) ; JavaScript event as massaged by jQuery
  (define input #f)
  (define event-type #f)
  (define dragstart-target #f)
  (define dragover-target #f)
  (define drop-target #f)
  (define finished #f)
  (define run-button (getelem1 "dojo-button:contains('Run your program')"))
  (define keyword #f)
  (define elem #f)

  (js-set! run-button "pressed" #f)
  
  (while #t
    (begin
      (js-call% "setDraggability" (getelem1 ".draggables") "true")
      (with-handlers ((dragstart-handler ".draggables > dojo-button"))
	(set! input (get-input))
	(set! jq-event (second input)) ;; The generic parts of an event.
	(set! dragstart-target (js-ref jq-event "srcElement")))
      (js-call% "setDraggability" (getelem1 ".draggables") "false")

      (set! finished #f)
      (set! keyword #f)
      (while (not finished)
	(begin
	(with-handlers ((dragover-handler "#progbuffer")
			(dragleave-handler "#progbuffer")
			(drop-handler "#progbuffer"))
	  (set! input (get-input))
	  (set! event-type (js-ref (first input) "name"))
	  ;;(console-log (format #f "event-type => ~a" event-type))
	  (set! jq-event (second input)) ;; The generic parts of an event.
	  (case event-type
	    (("dragover")
	     (js-invoke jq-event "preventDefault")
	     (js-invoke jq-event "stopPropagation")
	     (set! dragover-target (js-ref jq-event "srcElement"))
	     (element-add-class-name! dragover-target "dragover")
	     (js-set! (get-data-transfer-obj jq-event) "dropEffect" "copy"))
	    (("dragleave")
	     (set! dragover-target (js-ref jq-event "target"))
	     (element-remove-class-name! dragover-target "dragover"))
	    (("drop")
	     (js-invoke jq-event "preventDefault")
	     (set! drop-target (js-ref jq-event "target"))
	     (element-remove-class-name! dragover-target "dragover")
	     (console-log (format #f "drop: drop-target => ~a" (js-ref drop-target "outerHTML")))
	     (set! keyword (js-ref dragstart-target "innerText"))
	     ;;(console-log (format #f "keyword => ~a" keyword))
	     (if (eq? keyword "=")
		 (begin
		   (js-call% "appendHTML" drop-target "<div class='ex'>= </div>")
		   (set! elem (js-ref drop-target "lastChild")) ; FIXME: Why won't lastChildElement work?
		   (console-log (format #f "drop: elem => ~a" (js-ref elem "outerHTML")))
		   (js-set! elem "innerHTML"
			    (string-append (js-ref elem "innerHTML")
					   (get-word elem "Enter argument here, or cancel using ESCAPE.")))
		   (js-set! elem "innerHTML"
			    (string-append (js-ref elem "innerHTML")
					   " " (get-word elem "Enter argument here, or cancel using ESCAPE."))))
		 (js-call% "appendHTML"
			   drop-target
			   (string-append "<div class='ex'>" keyword "</div>")))
	     (set! finished #t)))))))))

;; dataTransfer is part of a dragstart event, but not part of a generic jQuery event.
(define (get-data-transfer-obj jquery-event)
  ;; FIXME: Is it really necessary to dereference twice?
  (js-ref (js-ref jquery-event "originalEvent") "dataTransfer"))
