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

;; This just tests for ELEMENT_NODE and TEXT_NODE, but there are many
;; other types of node that should be handled. See https://mzl.la/2zcB5om.
(define (dom->sexp node)
  (case (node-type node)
    ((1)
     (cons
      (element->sexp node)
      (children->sexp (js-child-nodes node))))
    ((3)
     (js-ref node "data"))
    (else (node-name node))))

(define (dom->string node)
  (format "~s" (dom->sexp node)))

;;;; DOM deserialization

(define (text-node-new node)
  (js-invoke (js-eval "document.createTextNode") node))

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
