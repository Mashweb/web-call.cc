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

;; Maybe not working.
(define (children->string nodes)
  (display "---> Entering children->string")
  (display nodes)
  (console-dir nodes)
  (cond
   ((and nodes (list? nodes) (> (length nodes) 0))
    (display "Got non-empty list")
    (dom->string (car nodes))
    (children->string (cdr nodes)))
   (else
    (display "Error: children->string called with empty list or non-list"))))

;; Maybe not working.  
(define (dom->string node)
  (display "----> Entering dom->string")
  (display node)
  (console-dir node)
  (cond
   ((list? node)
    (display "Error: node->string called with list"))
   (else
    (display (string-append "nodeName: " (node-name node)))
    (children->string (js-child-nodes node))
    (node-name node))))
