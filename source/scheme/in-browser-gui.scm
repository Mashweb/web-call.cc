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

(load "scheme/browser-util.scm")
(load "scheme/config.scm")
;;(console-log (format #f "zen-db => " zen-db))

(define (message str)
  ;;(console-log (format #f "Message: ~a" str))
  (js-set! (getelem1 "#message") "innerHTML"
	   (string-append "<span class='message'>" str "</span>")))

(define (test)
  (let loop1
      ((op #f)
       (selector #f)
       (drop-effect #f)
       (unfinished #t))
    
    (message "Press one of the DOM-edit-tool green buttons below to edit the part of the DOM in the gray-shaded area of the page. (Currently the bluish gray buttons are disabled.)")
    (let loop1a
	()
      (with-handlers ((click-handler "#add")
		      (click-handler "#copyx")
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
	  (set! op (js-ref (js-ref jquery-event "currentTarget") "innerText"))
	  (case op
	    (("Copy-insert")
	     (set! drop-effect "copy")
	     (set! op "copy"))
	    (("Copy-before")
	     (set! drop-effect "copy")
	     (set! op "copybefore"))
	    (("Move-insert")
	     (set! drop-effect "move")
	     (set! op "move"))
	    (("Move-before")
	     (set! drop-effect "move")
	     (set! op "movebefore"))
	    (("Save")
	     (http-post-text (string-append zen-db "/doms")
			     (dom->string (getelem1 "div#draggables"))))
	    (else
	     (message "Unimplemented operation. Please press a different DOM-edit-tool button.")
	     (loop1a))))))

    (message "Now try dragging and dropping an HTML element.")
	
    (while unfinished
      (with-handlers ((dragstart-handler "#draggables")
		      (dragover-handler "#draggables")
		      (dragleave-handler "#draggables")
		      (drop-handler "#draggables"))
	(let* ((input (get-input))
	       (event-type (js-ref (first input) "name"))
	       (jquery-event (second input)) ;; The generic parts of an event.
	       (dragged #f)
	       (dragover-target #f)
	       (dragleave-target #f))
	  (case event-type
	    (("dragstart")
	     (set! dragged (js-ref jquery-event "target"))
	     (console-log (format #f "dragged tagName => ~a" (js-ref dragged "tagName")))
	     (set! selector (js-call% "finder" dragged))
	     (js-invoke (get-data-transfer-obj jquery-event) "setData" "text/plain" selector)
	     (element-add-class-name! dragged "dragged"))
	    (("dragover")
	     (js-invoke jquery-event "preventDefault")
	     (js-invoke jquery-event "stopPropagation")
	     (set! dragover-target (js-ref jquery-event "srcElement"))
	     (element-add-class-name! dragover-target "dragover")
	     ;; Assume op is "Copy" or "Move".
	     (js-set! (get-data-transfer-obj jquery-event) "dropEffect" drop-effect))
	    (("dragleave")
	     (set! dragover-target (js-ref jquery-event "target"))
	     (element-remove-class-name! dragover-target "dragover"))
	    (("drop")
	     (js-invoke jquery-event "preventDefault")
	     (unless (perform-operation op jquery-event)
	       (loop1a))
	     (message (format #f "~a operation complete." op))
	     (set! unfinished #f))))))
    (loop1)))

;; dataTransfer is part of a dragstart event, but not part of a generic jQuery event.
(define (get-data-transfer-obj jquery-event)
  ;; FIXME: Is it really necessary to dereference twice?
  (js-ref (get-original-event jquery-event) "dataTransfer"))

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
