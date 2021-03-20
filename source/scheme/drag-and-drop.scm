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

;; draggability-on should be either "true" or "false".
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
	  (set! dragged (target jq-event))
	  (set! selector (js-call% "finder" dragged))
	  (js-invoke (get-data-transfer-obj jq-event) "setData" "text/plain" selector)
	  (element-add-class-name! dragged "dragged")
	  dragged)
	#f))) ;; #f indicates the drag operation was cancelled.

;; Finish a drag-and-drop operation and return the drop target and its left and top coordinates.
;; Return #f if the operation is cancelled. FIXME: Check the return value for a cancellation.
(define (drop dragged to-dom)
  (let ((to (getelem1 "#to"))
	(input #f)
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
	   (prevent-default jq-event)
	   (stop-propagation jq-event)
	   (set! dragover-target (src-element jq-event))
	   ;;(element-add-class-name! dragover-target "dragover")
	   (element-add-class-name! to "dragover")
	   (js-set! (get-data-transfer-obj jq-event) "dropEffect" "copy"))
	  (("dragleave")
	   (set! dragover-target (target jq-event))
	   (element-remove-class-name! dragover-target "dragover"))
	  (("dragend")
	   (element-remove-class-name! dragged "dragged"))
	  (("drop")
	   (prevent-default jq-event)
	   ;;(element-remove-class-name! dragover-target "dragover")
	   (element-remove-class-name! to "dragover")
	   (set! left (page-x jq-event))
	   (set! top (page-y jq-event))
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
	  ;;(js-invoke to "appendChild" (clone-dom dragged))
	  (append-child (clone-dom dragged) to)
	  #t)
	#f)))

(define (main)
  (while (add-charts)))
