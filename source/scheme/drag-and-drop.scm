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

(load "scheme/browser-util.scm")

(define (set-draggability draggables-selector draggability-on)
  (map (lambda (element)
	 (element-write-attribute! element "draggable" draggability-on))
       (js-array->list (js-invoke (getelem draggables-selector) "toArray"))))

;; Start a drag-and-drop operation and return the dragstart target.
;; Return #f if the operation is cancelled.
(define (dragstart from-dom draggables-selector cancel-selector)
  (let ((input #f)
	(jq-event #f) ;; An event in jQueryland
	(dragged #f)
	(selector #f))
    (set-draggability draggables-selector "true")
    (with-handlers-1 `((,'click-handler ,cancel-selector)
		       (,'dragstart-handler ,from-dom))
      (set! input (get-input)))
    (set-draggability draggables-selector "false")    
    (if (eqv? (js-ref (first input) "name") "dragstart") ;; event type
	(begin
	  (set! jq-event (second input)) ;; The generic parts of an event.
	  (set! dragged (js-ref jq-event "target"))
	  (set! selector (js-call% "finder" dragged))
	  (format #t "selector => ~a" selector)
	  (js-invoke (get-data-transfer-obj jq-event) "setData" "text/plain" selector)
	  (element-add-class-name! dragged "dragged")
	  dragged)
	#f))) ;; #f indicates the drag operation was cancelled.

;; Start a drag-and-drop operation and return the dragstart target.
;; Return #f if the operation is cancelled.
(define (drop dragged to-dom)
  (let ((input #f)
	(jq-event #f) ;; An event in jQueryland
	(dragover-target #f)
	(unfinished #t)
	(original-event #f)
	(left 0)
	(top 0))
    (while unfinished
      (begin
	(with-handlers ((dragover-handler "#to") ; FIXME: See https://web.dev/drag-and-drop/
			(dragleave-handler "#to")
			(drop-handler "#to")
			(dragend-handler "body"))
	  (set! input (get-input)))
	(set! jq-event (second input))
	(case (js-ref (first input) "name") ;; event type
	  (("dragover")
	   (js-invoke jq-event "preventDefault")
	   (js-invoke jq-event "stopPropagation")
	   (set! dragover-target (js-ref jq-event "srcElement"))
	   (element-add-class-name! dragover-target "dragover")
	   (js-set! (get-data-transfer-obj jq-event) "dropEffect" "copy"))
	  (("dragleave")
	   (set! dragover-target (js-ref jq-event "target"))
	   (element-remove-class-name! dragover-target "dragover"))
	  (("dragend")
	   (element-remove-class-name! dragged "dragged"))
	  (("drop")
	   (js-invoke jq-event "preventDefault")
	   (element-remove-class-name! dragover-target "dragover")
	   (set! unfinished #f)
	   (set! original-event (get-original-event jq-event))
	   (set! left (js-ref original-event "pageX"))
	   (set! top (js-ref original-event "pageY"))
	   (set! unfinished #f)))))
    (list (js-ref jq-event "target") left top))) ;; Drop target; left and top placement

(define (add-charts)
  (let* ((dragged (dragstart "#from" ".chart" "#cancel"))
	 (drop-result #f)
	 (to (getelem1 "#to")))
    (if (not (eqv? dragged #f))
	(begin
	  (set! drop-result (drop dragged "#to"))
	  (element-remove-class-name! dragged "dragged")
	  ;;(js-invoke (first drop-result) "appendChild" (js-call% "cloneDOM" dragged))
	  (js-invoke to "appendChild" (js-call% "cloneDOM" dragged))
	  #t)
	#f)))

(define (main)
  (while (add-charts)))

(reset (main))
