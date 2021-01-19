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
    (map (lambda (element)
    	   (element-write-attribute! element "draggable" "true"))
    	 (js-array->list (js-invoke (getelem draggables-selector) "toArray")))
    (with-handlers `((,'click-handler ,cancel-selector)
		     (,'dragstart-handler ,from-dom))
      (set! input (get-input)))
    (format #t "Got input => ~a" input)
    (map (lambda (element)
    	   (element-write-attribute! element "draggable" "false"))
    	 (js-array->list (js-invoke (getelem draggables-selector) "toArray")))
    (set! event-type (js-ref (first input) "name"))
    (if (eqv? event-type "dragstart")
	(begin
	  (set! jq-event (second input)) ;; The generic parts of an event.
	  (js-ref (second input) "srcElement")) ;; The dragstart target.
	#f))) ;; #f indicates the drag operation was cancelled.

(format #t "Defined dragstart")

(define (call-dragstart)
  (format #t "Calling dragstart")
  (format #t "(dragstart ...) => ~a" (dragstart "#from" ".chart" "#cancel")))

(define (main)
  (console-log "Hello, world!"))
