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
(define (test)
  (let loop1
      ((op #f)
       (target #f)
       (selector #f)
       (unfinished #t))
    
    (message "Please choose a DOM-editor tool. (Currently only Copy-insert and Move-insert are working.)")
    (let loop1a
	()
      (with-handlers ((click-handler "#add")
		      (click-handler "#copy")
		      (click-handler "#copy-before")
		      (click-handler "#paste")
		      (click-handler "#move")
		      (click-handler "#move-before")
		      (click-handler "#delete")
		      (click-handler "#undo")
		      (click-handler "#redo")
		      (click-handler "#save"))
	(let* ((input (get-input))
	       (event-type (js-ref (first input) "name"))
	       (jquery-event (second input)))
	  (console-dir jquery-event)
	  (set! op (js-ref (js-ref jquery-event "currentTarget") "innerText"))
	  (console-log (format #f "op => ~a" op))
	  (case op
	    (("Copy-insert")
	     (set! op "copy"))
	    (("Copy-before")
	     (set! op "copybefore"))
	    (("Move-insert")
	     (set! op "move"))
	    (("Move-before")
	     (set! op "movebefore"))
	    (else
	     (message "Unimplemented operation. Please choose a different DOM-editor tool.")
	     (loop1a))))))

    (message "Now try dragging and dropping an HTML element.")
	
    (while unfinished
      (with-handlers ((dragstart-handler "#canvas")
		      (dragover-handler "#canvas")
		      (drop-handler "#canvas"))
	(let* ((input (get-input))
	       (event-type (js-ref (first input) "name"))
	       (jquery-event (second input))) ;; The generic parts of an event.
	  (case event-type
	    (("dragstart")
	     (console-log "dragstart")
	     (set! target (js-ref jquery-event "target"))
	     (console-log target)
	     ;; FIXME: This means of calling select looks more complicated than necessary.
	     (set! selector (js-invoke (js-eval "OptimalSelect") "select" target))
	     (console-log (format #f "selector => ~a" selector))
	     (js-invoke (get-data-transfer-obj jquery-event) "setData" "text/plain" selector))
	    (("dragover")
	     (js-invoke jquery-event "preventDefault")
	     (js-invoke jquery-event "stopPropagation")
	     ;; Assume op is "Copy" or "Move".
	     (js-set! (get-data-transfer-obj jquery-event) "dropEffect" (string-downcase op))
	     ;;(console-dir jquery-event)
	     )
	    (("drop")
	     (console-log "drop")
	     (js-invoke jquery-event "preventDefault")
	     (unless (perform-operation op jquery-event)
	       (loop1a))
	     (message "Operation complete.")
	     (console-log "Operation complete.")
	     (set! unfinished #f))))))
    (loop1)))

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
	 (dragged (getelem1 (js-invoke (get-data-transfer-obj jq-ev) "getData" "text/plain")))
	 (parent (js-ref target "parentNode")))
    (console-log (format #f "Operation: ~a" op))
    (console-dir jq-ev)
    (console-log (format #f "target: ~a" target))
    (console-log (format #f "dragged: ~a" dragged))
    (console-log (format #f "parent: ~a" parent))
    (case op
      (("copy")
       (js-invoke target "appendChild" (js-invoke dragged "cloneNode" "true")))
      (("copybefore")
       (js-invoke parent "insertBefore" (js-invoke dragged "cloneNode" "true") target))
      (("move")
       (js-invoke target "appendChild" dragged))
      (("movebefore")
       (js-invoke parent "insertBefore" dragged target)))))

       
