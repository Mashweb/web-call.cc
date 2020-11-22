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

;;;; Arities (min max) for various Scheme functions, macros, and datums; 2 meaning at least 2; -1 meaning any number.

(define arities (make-eqv-hashtable))
(define (arity str) (first (hashtable-ref arities str #f)))

;; Forms

(hashtable-set! arities "define" '(1 2))
(hashtable-set! arities "function" '(2 2))
(hashtable-set! arities "lambda" '(0 -1))
(hashtable-set! arities "set!" '(2 2))
(hashtable-set! arities "begin" '(1 -1))
(hashtable-set! arities "not" '(1 1))
(hashtable-set! arities "=" '(2 -1))
(hashtable-set! arities "alert" '(1 1))

;; Datums

(hashtable-set! arities "#t" '(0 0))
(hashtable-set! arities "#f" '(0 0))

(define (test)
  (define jq-event #f) ; JavaScript event as massaged by jQuery
  (define input #f)
  (define event-type #f)
  (define op-target #f)
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
      (with-handlers ((dragstart-handler ".draggables > dojo-button")
		      (click-handler "#RunButton"))
        (set! input (get-input))
        (set! jq-event (second input)) ;; The generic parts of an event.
        (set! op-target (js-ref jq-event "srcElement")))
      (js-call% "setDraggability" (getelem1 ".draggables") "false")
      (console-log (format #f "event => ~a; op-target => ~a" (first input) (get-inner-html op-target)))
      (if (eqv? "click" (symbol->string (first input)))
	  (save-program)
	  (begin
	    (set! finished #f)
	    (set! keyword #f)
	    (while (not finished)
	      (with-handlers ((dragover-handler "#progbuffer")
			      (dragleave-handler "#progbuffer")
			      (drop-handler "#progbuffer"))
		(set! input (get-input))
		(set! event-type (js-ref (first input) "name"))
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
		   (set! keyword (js-ref op-target "innerText"))
		   (case (arity keyword)
		     ((0)
		      (append-html! drop-target (string-append keyword)))
		     ((1)
		      (append-html! drop-target (string-append "<div class='ex'>" keyword " </div>"))
		      (set! elem (js-ref drop-target "lastChild")) ; FIXME: Why won't lastChildElement work?
		      (append-to-inner-html! elem (get-sym/datum elem "Type/drag argument here or cancel using ESC.")))
		     ((2)
		      (append-html! drop-target (string-append "<div class='ex'> " keyword " </div>"))
		      (set! elem (js-ref drop-target "lastChild")) ; FIXME: Why won't lastChildElement work?
		      (append-to-inner-html! elem (get-sym/datum elem "Type/drag argument here or cancel using ESC."))
		      (append-to-inner-html! elem (get-sym/datum elem "Type/drag argument here or cancel using ESC."))))
		   (set! finished #t))))))))))

(define (DIVx arg)
  (console-log-fmt "DIV: arg => ~a" arg)
  (lambda (var)
    (console-log-fmt "lambda: var => ~a" var)))

(define (classx arg)
  (console-log-fmt "class: arg => ~a" arg)
  (lambda (var)
    (console-log-fmt "lambda: var => ~a" var)))

(define (save-program)
  ;;(console-log (format #f "Program: ~a" (dom->string (getelem1 "#progbuffer"))))
  (map (lambda (element)
	 (let ((intermediate-code #f)
	       (got-paren #f)
	       (final-code '()))
	   (set! intermediate-code (dom->sexp element))
	   (console-log (format #f "intermediate-code => ~a" intermediate-code))
	   (if (list? intermediate-code)
	       (begin
		 (console-log-fmt "(car intermediate-code) => ~a" (car intermediate-code))
		 (console-log-fmt "(cdr intermediate-code) => ~a" (cdr intermediate-code)))
		 (console-log "Calling map...")
		 (map (lambda (part)
			(console-log (format #f "part => ~a"
					     (if got-paren (string-append "(" part)
						 part)))
			(if (string=? part "(DIV (class ex))")
			    (set! got-paren #t)
			    (append final-code part))
			intermediate-code))
		 (eval final-code)
	       (begin
		 (console-log "Calling string->sexp...")
		 (console-log-fmt "(eval intermediate-code) => ~a" (eval intermediate-code))))))
       (js-child-nodes (getelem1 "#progbuffer"))))
