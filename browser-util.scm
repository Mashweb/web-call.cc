(load "mini-framework.scm")

;;;; Console

;; To call the browser function 'console.dir', define a Scheme function:
(define console-dir (js-ref (js-eval "console") "dir"))

;;;; DOM navigation

(define (js-childNodes node)
  (js-ref node "childNodes"))

(define (js-child-nodes node)
  (js-array->list (js-childNodes node)))

;;;; DOM serialization

(define (node-name node)
  (display "--> Entering node-name")
  (display (string-append "nodeName: " (js-ref node "nodeName")))
  (js-ref node "nodeName"))

(define q '())

;; Something in children->string or dom->string causes infinite loop or recursion.

(define (children->string nodes)
  (display "---> Entering children->string")
  (display nodes)
  (console-dir nodes)
  (cond
   ((null? nodes)
    (display "Got an empty list"))
   ((list? nodes)
    (display "Got non-empty list")
    (dom->string (car nodes))
    (display "(cdr nodes) => ")
    (display (cdr nodes))
    (children->string (cdr nodes))) ;; No effect?
   (else
    (display "Error: children->string called with empty list or non-list"))))

(define (dom->string node)
  (display "----> Entering dom->string")
  (display node)
  (console-dir node)
  (cond
   ((list? node)
    (display "Error: node->string called with list"))
   (else
    ;;(display (string-append "nodeName: " (node-name node)))
    (children->string (js-child-nodes node)) ;; No effect?
    (push! (node-name node) q))))
