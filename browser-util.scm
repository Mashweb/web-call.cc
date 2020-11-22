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

(load "mini-framework.scm")

;;;; Console and messages

(define (console-log-fmt str obj)
  (console-log (format #f str obj)))

(define (console-dir obj)
  (js-call% "console.dir" obj))

(define (console-group str)
  (js-call% "console.group" str))

(define (console-group-end)
  (js-call% "console.groupEnd"))

;;;; AJAX

(js-eval "define_libfunc('http-post-text', 2, 2, function(ar){
    var path = ar[0];
    assert_string(path);
    var data = ar[1];
    assert_string(data);

    return new BiwaScheme.Pause(function(pause){
      $.ajax({
        'type': 'POST',
        'url': path,
        'contentType': 'text/plain',
        'data': data,
        'dataType': 'text/plain',
        'success': pause.resume(data)
      });
    });
  })")

;;;; DOM navigation

;; Get the first element matching the selector.
(define (getelem1 selector)
  (js-ref (getelem selector) "0"))

(define (js-childNodes node)
  (js-ref node "childNodes"))

(define (js-child-nodes node)
  (js-array->list (js-childNodes node)))

(define (js-children element)
  (js-ref element "children"))

(define (js-children-list element)
  (js-array->list (js-children element)))

;;;; DOM serialization

(define (node-type node)
  (js-ref node "nodeType"))

(define (node-name node)
  (string->symbol (js-ref node "nodeName")))

(define (element-attributes element)
  (let* ((attrs (js-ref element "attributes"))
         (attr-list (js-array->list attrs)))
    (map (lambda (attr-node)
           (list (string->symbol (js-ref attr-node "name"))
                 (js-ref attr-node "value")))
         attr-list)))

(define (children->sexp nodes)
  (map dom->sexp nodes))

(define (element->sexp element)
  (cons (node-name element) (element-attributes element)))

(define (dom->string node)
  (format "~s" (dom->sexp node)))

;; FIXME:
;; This just tests for ELEMENT_NODE and TEXT_NODE, but there are many
;; other types of node that should be handled. See https://mzl.la/2zcB5om.
(define (dom->sexp node)
  (case (node-type node)
    ((1) ;; Node.ELEMENT_NODE
     (cons
      (element->sexp node)
      (children->sexp (js-child-nodes node))))
    ((3) ;; Node.TEXT_NODE
     (js-ref node "data"))
    (else (node-name node))))

;; Example HTTP request using the output of dom->string:
;; (http-post "/doms" '(("dom" . "((DIV (id \"junk\") (style \"background-color:yellow;\")) \"      \" ((OL) \"       
;; \" ((LI) \"One\") \"        \" ((LI) \"Two\") \"        \" ((LI) \"Three\") \"      
;; \") \"    \")")))

;;;; DOM deserialization

;; FIXME: This doesn't handle input like "\n   ".
(define (text-node-new node)
  (display (string-append "text-node-new: " node))
  (display (string-length node))
  (js-eval (string-append "document.createTextNode('" node "')")))

(define (string->sexp str)
  (read (open-input-string str)))

(define (symbol->element part)
  (display part)
  (cond
   ((list? part)
    (display "symbol->element: got a list")
    (element-new (car part)))
   (else
    (display "symbol->element: got a non-list")
    ;; This doesn't work, probably because it's not quoted for JavaScript:
    (text-node-new part))))

(define (sexp->dom sexp)
  (map symbol->element sexp))

;;;; DOM operations cognisant of custom elements: find target, clone, move, append, insert
;;
;; The HTML API's cloneNode method cannot be used for copying DOMs because:
;;   * Some custom elements, such as Dojo widgets, are not wholly draggable,
;;     so that when the user clicks on one of them, the target of the
;;     dragstart event does not contain the whole widget.
;;   * The id attribute of an element should not be used twice.

(define (clone-dom target)
  (js-call% "cloneDOM" target))

;;;; Input

;; get-sym/datum - get a Scheme symbol or datum via keyboard or by dragging an HTML element representing a symbol
;;
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
(define (get-sym/datum whence prompt)
  (let ((textarea (element-new '(textarea id "word" rows "1" cols "42"))))
    (element-append-child! whence textarea)
    (sleep 0.1) ;; FIXME
    (js-set! textarea "value" prompt)
    (js-invoke textarea "focus")
    (js-set! textarea "selectionEnd" 0)
    (with-handlers ((keyup-handler "#word")
		    (dragstart-handler ".draggables > dojo-button"))
      (let ((input #f)
	    (char #f)
            (finished #f)
            (aborted #f)
            (str #f)
            (real-num #f)
            (first-char #t)
            (got-sym #f))

        ;; Get and examine the first character or dragged HTML element

	(set! input (get-input))
	(console-log (format #f "Got event => ~a" (first input)))
	;;(if (string=? "key")
	;;    (begin


        (set! char (event->charcode (second input)))
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

                   ;; FIXME: Use cond instead of case?
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

(define (append-html! elem str)
  (js-call% "appendHTML" elem str))

(define (get-inner-html elem)
  (js-ref elem "innerHTML"))

(define (set-inner-html! elem str)
  (js-set! elem "innerHTML" str))

(define (append-to-inner-html! elem str)
  (js-set! elem "innerHTML" (string-append (js-ref elem "innerHTML") str " ")))

;; dataTransfer is part of a dragstart event, but not part of a generic jQuery event.
(define (get-data-transfer-obj jquery-event)
  ;; FIXME: Is it really necessary to dereference twice?
  (js-ref (js-ref jquery-event "originalEvent") "dataTransfer"))
