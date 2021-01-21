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

;; Start a drag-and-drop operation and return the dragstart target.
;; Return #f if the operation is cancelled.
(define (dragstart from-dom draggables-selector cancel-selector)
  (let ((input #f)
	(jq-event #f) ;; An event in jQueryland
	(event-type #f)
	(dragstart-target #f))
    (format #t "(dragstart ~a ~a ~a)" from-dom draggables-selector cancel-selector)
    (map (lambda (element)
    	   (element-write-attribute! element "draggable" "true"))
    	 (js-array->list (js-invoke (getelem draggables-selector) "toArray")))
    (with-handlers-1 `((,'click-handler ,cancel-selector)
		       (,'dragstart-handler ,from-dom))
      (set! input (get-input)))
    (format #t "Got input => ~a" input)
    ;;(map (lambda (element)
    ;;	   (element-write-attribute! element "draggable" "false"))
    ;;	 (js-array->list (js-invoke (getelem draggables-selector) "toArray")))
    (set! event-type (js-ref (first input) "name"))
    (format #t "event-type => ~a" event-type)
    (if (eqv? event-type "dragstart")
	(begin
	  (set! jq-event (second input)) ;; The generic parts of an event.
	  (js-ref (second input) "srcElement")) ;; The dragstart target.
	#f))) ;; #f indicates the drag operation was cancelled.

;; Start a drag-and-drop operation and return the dragstart target.
;; Return #f if the operation is cancelled.
(define (drop to-dom)
  (let ((input #f)
	(jq-event #f) ;; An event in jQueryland
	(event-type #f)
	(dragover-target #f)
	(unfinished #t))
    (format #t "(drop ~a)" to-dom)
    ;;(with-handlers-1 `((,click-handler ,cancel-selector))
    ;;(with-handlers ((click-handler "#cancel"))
    ;;(with-handlers ((dragover-handler ".chart"))
    ;;(with-handlers ((dragover-handler ,draggables-selector)) ;;Doesn't work.
    ;;(with-handlers ((dragover-handler "#from"))
    
    (map (lambda (element)
    	   (element-write-attribute! element "draggable" "true"))
    	 (js-array->list (js-invoke (getelem ".chart") "toArray")))

    (format #t "Mapped draggable elements")

    ;;(while unfinished
	   ;;(begin
    (format #t "Top of loop")
    (with-handlers (;;(dragstart-handler "#to")
		    (dragover-handler "#to")
		    (dragleave-handler "#to")
		    (drop-handler "#to"))
      (format #t "Inside with-handlers form")
      (set! input (get-input)))
    (format #t "Got input => ~a" input)
    (set! jq-event (second input))
    (set! event-type (js-ref (first input) "name"))
    (format #t "jq-event => ~a; event-type => ~a" jq-event event-type)))
#|    (case event-type
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
       (js-set! (get-data-transfer-obj jquery-event) "dropEffect" drop-effect))
      (("dragleave")
       (set! dragover-target (js-ref jquery-event "target"))
       (element-remove-class-name! dragover-target "dragover"))
      (("drop")
       (js-invoke jquery-event "preventDefault")
       (format #t "Dropped."))
       (set! unfinished #f))))
|#

(define (add-charts)
  (let ((dragstart-target (dragstart "#from" ".chart" "#cancel"))
	(drop-target #f))
    (format #t "dragstart-target => ~a" dragstart-target)
    (if (not (eqv? dragstart-target #f))
	(begin
	  (drop "#to"))
	#f)))

(define (main)
  (console-log "Hello, world!"))

(format #t "Ready.")
