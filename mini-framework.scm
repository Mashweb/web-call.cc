
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

;; This is from the unit tests (https://github.com/biwascheme/biwascheme/blob/master/test/unit.js):
;;(let1 ls '() (dotimes (x 3 ls) (set! ls (cons x ls))))

;; This almost works.
;;(dotimes '(print 1 5))

;; Define a simple iterator (which actually uses tail-call rescursion):
;; Adaptation of the factorial at https://wiki.c2.com/?SchemeIdioms .
;; See also _ANSI Common Lisp_, p. 164 and _Teach Yourself Scheme_, Chapter 8.
(define-macro (repeat n . body)
  (let ((i (gensym)))
    `(let loop ((,i 1))
       ,@body
       (if (< ,i ,n)
           (loop (+ 1 ,i))))))

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
(define-event-handler dragstart-handler dragstart)
(define-event-handler dragover-handler dragover)
(define-event-handler drop-handler drop)

;;;; get-input

(define-macro (get-input)
  `(shift c (set! input-listener-cont c)))
