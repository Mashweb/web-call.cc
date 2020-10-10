; Copyright (C) 2020 Thomas Elam
;
; This file is part of web-call.cc.
;
; web-call.cc is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; web-call.cc is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with web-call.cc.  If not, see <https://www.gnu.org/licenses/>.

(load "browser-util.scm")

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))
  
(define (test)
  (define jquery-event #f)
  (define input #f)
  (define event-type #f)
  (define dragstart-target #f)
  (define dragover-target #f)
  (define drop-target #f)
  (define finished #f)
  (define run-button (getelem1 "dojo-button:contains('Run my program')"))
  (define atom #f)

  (js-set! run-button "pressed" #f)
  
  (while #t
    (begin
      (js-call% "setDraggability" (getelem1 ".draggables") "true")
      (with-handlers ((dragstart-handler ".draggables > dojo-button"))
	(set! input (get-input))
	(set! jquery-event (second input)) ;; The generic parts of an event.
	(set! dragstart-target (js-ref jquery-event "srcElement")))
      (js-call% "setDraggability" (getelem1 ".draggables") "false")

      (set! finished #f)
      (while (not finished)
	(with-handlers ((dragover-handler "#progbuffer")
			(dragleave-handler "#progbuffer")
			(drop-handler "#progbuffer"))
	  (set! input (get-input))
	  (set! event-type (js-ref (first input) "name"))
	  (console-log (format #f "event-type => ~a" event-type))
	  (set! jquery-event (second input)) ;; The generic parts of an event.
	  (case event-type
	    (("dragover")
	     (js-invoke jquery-event "preventDefault")
	     (js-invoke jquery-event "stopPropagation")
	     (set! dragover-target (js-ref jquery-event "srcElement"))
	     (element-add-class-name! dragover-target "dragover")
	     (js-set! (get-data-transfer-obj jquery-event) "dropEffect" "copy"))
	    (("dragleave")
	     (set! dragover-target (js-ref jquery-event "target"))
	     (element-remove-class-name! dragover-target "dragover"))
	    (("drop")
	     (js-invoke jquery-event "preventDefault")
	     (set! drop-target (js-ref jquery-event "target"))
	     (element-remove-class-name! dragover-target "dragover")
	     (console-log (format #f "drop: drop-target => ~a" drop-target))
	     (set! atom (js-ref dragstart-target "innerText"))
	     (js-call% "appendHTML"
		       drop-target
		       (string-append "<div class='ex'>" (js-ref dragstart-target "innerText") "</div>"))
	     (if (eq? atom "=")
		 (input-2-numbers drop-target)
		 (console-log (format #f "Picked ~a" atom)))
	     (console-log "Dropped ...")
	     (set! finished #t))))))))

(define (input-2-numbers target)
  (console-log "input-2-numbers")
  ;;(js-eval "document.body.focus()")
  ;;(js-eval "$(document).on('keyup', function (e) { console.log(e.keyCode); });")
  ;;(console-log "After js-eval")
  (with-handlers ((keypress-handler "body"))
    (console-log (get-input))
    (console-log (get-input)))
  (console-log "input-2-numbers finished"))

;; originalEvent is referenced by a jQuery event. It is what it says: the original event,
;; unmassaged by jQuery.
(define (get-original-event jquery-event)
  (js-ref jquery-event "originalEvent"))

;; dataTransfer is part of a dragstart event, but not part of a generic jQuery event.
(define (get-data-transfer-obj jquery-event)
  ;; FIXME: Is it really necessary to dereference twice?
  (js-ref (js-ref jquery-event "originalEvent") "dataTransfer"))

(define (perform-operation op jq-ev)
  (let* ((target (js-ref jq-ev "target"))
	 (dragged-selector (js-invoke (get-data-transfer-obj jq-ev) "getData" "text/plain"))
	 (dragged (getelem1 dragged-selector))
	 ;;(parent (js-ref target "parentNode"))
	 (parent (js-ref (js-call% "findTrueTarget" target) "parentNode")) ;; TODO: Check whether this is correct.
	 (is-custom-element #f)
	 (tagName ""))
    (console-log (format #f "perform-operation ~a" op))
    (element-remove-class-name! dragged "dragged")
    (element-remove-class-name! target "dragover")
    (console-log (format #f "dragged-selector => ~a" dragged-selector))
    (case op
      (("copy")
       (console-log "copy")
       (js-invoke target "appendChild" (js-call% "cloneDOM" dragged)))
      (("copybefore")
       (begin
	 ;;(console-dir (js-call% "cloneDOM" dragged))
	 ;;(console-log "called console-dir")
	 (console-log "parent:")
	 (console-dir parent)
	 (console-log "target:")
	 (console-dir target)
	 (js-invoke parent "insertBefore" (js-call% "cloneDOM" dragged) (js-call% "findTrueTarget" target))
	 ))
      (("move")
       (if (js-invoke dragged "contains" target)
	   (alert "Invalid operation: dragged element contains itself.")
	   (js-invoke target "appendChild" dragged)))
      (("movebefore")
       (if (js-invoke dragged "contains" parent)
	   (alert "Invalid operation: dragged element contains itself.")
	   (js-invoke parent "insertBefore" dragged target))))))
