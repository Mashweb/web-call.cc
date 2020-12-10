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
;; along with web-call.Cc.  If not, see <https://www.gnu.org/licenses/>.

;; Everything in this file below this sentence except for the function nth
;; and the macro str-rm-last-char! was programmed by Alexander Sukhoverkhov
;; (naryl.pandora@gmail.com).

;;;; shift-reset

(define-macro (reset . prog)
  `(*reset (lambda () ,@prog)))

(define-macro (shift arg . prog)
  `(*shift (lambda (,arg) ,@prog)))

(define (*meta-continuation* v)
  (error "You forgot the top-level reset..."))

(define (*abort thunk)
  (let ((v (thunk)))
    (*meta-continuation* v)))

(define (*reset thunk)
  (let ((mc *meta-continuation*))
    (call-with-current-continuation
     (lambda (k)
       (set! *meta-continuation*
             (lambda (v)
               (set! *meta-continuation* mc)
               (k v)))
       (*abort thunk)))))

(define (*shift f)
  (call-with-current-continuation
   (lambda (k)
     (*abort (lambda ()
               (f (lambda (v)
                    (reset (k v)))))))))

;;;; utils
;;;; (Some things I'm used to from Common Lisp)

(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)

;; Courtesy Sean Eshbaugh: https://bit.ly/2BRWvIN
(define (nth n l)
  (if (or (> n (length l)) (< n 0))
    (error "Index out of bounds.")
    (if (eq? n 0)
      (car l)
      (nth (- n 1) (cdr l)))))

(define rest cdr)

(define-macro (push! item place)
  `(set! ,place (cons ,item ,place)))

(define-macro (delete! item place)
  `(set! ,place (remove ,item ,place)))

(define (remove item list)
  (cond
   ((equal? item (car list))
    (cdr list))
   (else
    (cons (car list)
          (remove item (cdr list))))))

(define-macro (when cond . body)
  `(if ,cond (begin ,@body)))

(define-macro (unless cond . body)
  `(if (not ,cond) (begin ,@body)))

;;;; Some other utils

(define-macro (while test expr)
  `(let loop ()
     (when ,test
	   ,expr
	   (loop))))

(define (displayln text)
  (display text)
  (newline))

(define-macro (wind-protect setup teardown . body)
  (let ((result-g (gensym "RESULT")))
    `(begin
       ,setup
       (let ((,result-g (begin ,@body)))
         ,teardown
         ,result-g))))

;;;; JS stuff

(define-macro (js-call% func . args)
  `(js-call (js-eval ,func) ,@args))

(define-macro (js-lambda args . body)
  `(js-closure (lambda ,args ,@body)))

;;;; with-handlers

(define (input-listener-cont . args)
  (display "Handler called with no listener continuation up"))

(define (call-with-handlers handlers fn)
  (wind-protect
      (setup-handlers handlers)
      (remove-handlers handlers)
    (fn)))

(define-macro (with-handlers handlers . body)
  `(call-with-handlers ',handlers (lambda () ,@body)))

(define (setup-handlers handlers)
  (process-handlers handlers second))

(define (remove-handlers handlers)
  (process-handlers handlers third))

(define (process-handlers handlers part)
  (map (lambda (handler)
         (let ((handler-func (part (assq (first handler) handlers-impl)))
               (handler-args (rest handler)))
           (apply handler-func handler-args)))
       handlers))

(define (make-event-handler sym)
  (lambda (this event)
    (input-listener-cont (list sym this event))))

(define-macro (define-event-handler handler-name event-sym)
  (let* ((event-name (symbol->string event-sym))
         (handlers-container (string->symbol (string-append event-name "-handlers%")))
         (add-handler `(lambda (selector)
                         (let ((handler (add-handler! selector ,event-name
                                                      (make-event-handler ',event-sym))))
                           (hashtable-set! ,handlers-container selector handler))))
         (remove-handler `(lambda (selector)
                            (let ((handler (hashtable-ref ,handlers-container selector #f)))
                              (remove-handler! selector ,event-name handler)
                              (hashtable-delete! ,handlers-container selector)))))
    `(begin
       (define ,handlers-container (make-eqv-hashtable))
       (push! (list ',handler-name
                    ,add-handler
                    ,remove-handler)
              handlers-impl))))

(define ajax-handlers-container (make-eqv-hashtable))
(define timeout-handlers-container (make-eqv-hashtable))

(define handlers-impl
  (list
   (list 'timeout-handler
         (lambda (key timeout)
           (hashtable-set! timeout-handlers-container key
                           (js-call% "setTimeout"
                                    (js-lambda ()
                                      (input-listener-cont (list 'timeout key)))
                                    timeout)))
         (lambda (key timeout)
           (js-call% "clearTimeout"
                     (hashtable-ref timeout-handlers-container key #f))
           (hashtable-delete! timeout-handlers-container key)))
   (list 'ajax-handler
         (lambda (key url data)
           (hashtable-set! ajax-handlers-container key #t)
           (let ((ajax-func (js-eval "$.ajax")))
             (ajax-func (js-obj "url" url
                                "data" (apply js-obj data)
                                "success" (js-lambda (data)
                                            (if (hashtable-ref ajax-handlers-container key #f)
                                                (input-listener-cont (list key 'success data))))
                                "error" (js-lambda (jqxhr status error)
                                          (if (hashtable-ref ajax-handlers-container key #f)
                                              (input-listener-cont (list key 'error status error))))))))
         (lambda (key url data)
           (hashtable-delete! ajax-handlers-container key)))))

(define-event-handler click-handler click)
(define-event-handler keydown-handler keydown)
(define-event-handler keyup-handler keyup)
(define-event-handler keypress-handler keypress)
(define-event-handler dragstart-handler dragstart)
(define-event-handler dragover-handler dragover)
(define-event-handler dragleave-handler dragleave)
(define-event-handler drop-handler drop)

;;;; get user input

(define-macro (get-input)
  `(shift c (set! input-listener-cont c)))

;;;; strings

(define-macro (str-rm-last-char! str)
  `(begin
     (set! ,str (substring ,str 0 (- (string-length ,str) 1)))
     ,str))
