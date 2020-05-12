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

;; Currently we're only handling ELEMENT_NODEs and TEXT_NODEs. See https://mzl.la/2zcB5om.

(define (node-type node)
  (js-ref node "nodeType"))

(define (node-name node)
  (display "--> Entering node-name")
  (display (string-append "nodeName: " (js-ref node "nodeName")))
  (js-ref node "nodeName"))

(define (element-attributes element)
  (let* ((attrs (js-ref element "attributes"))
	 (attr-list (js-array->list attrs)))
    (map (lambda (attr-node) (list (js-ref attr-node "name") (js-ref attr-node "value")))
	 attr-list)))

(define q '())

;; Something in children->string or dom->string causes infinite loop or recursion.

(define (children->string nodes)
  (display "---> Entering children->string")
  (display nodes)
  (console-log nodes)
  (console-dir nodes)
  (cond ((> (length nodes) 1)
         (cons (node->string (car nodes))
               (children->string (cdr nodes))))
        (else
         (node->string (car nodes)))))

(define (dom->string node)
  (display "----> Entering dom->string")
  (let ((children (js-child-nodes node)))
    (cond ((and children (list? children) (> (length children) 0))
           (cons (element->string node)
                 (children->string children)))
          (else
           (node->string node)))))

(define (element->string element)
  (display "--> Entering element->string")
  (list (node-name element) (element-attributes element)))

;; This just tests for ELEMENT_NODE, but there are many other types
;; of node that should be handled. See https://mzl.la/2zcB5om.
;; Perhaps it would be better just to check the "attributes" property
;; of the node.
(define (node->string node)
  (display "--> Entering node->string")
  (cond ((equal? (node-type node) "1")
	 ;; Element
	 (element->string node))
	(else
	 (node-name node))))
