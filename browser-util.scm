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

(define (children->string nodes)
  (map dom->string nodes))

(define (element->string element)
  (cons (node-name element) (element-attributes element)))

;; This just tests for ELEMENT_NODE and TEXT_NODE, but there are many
;; other types of node that should be handled. See https://mzl.la/2zcB5om.
(define (dom->string node)
  (case (node-type node)
    ((1)
     (cons
      (element->string node)
      (children->string (js-child-nodes node))))
    ((3)
     (js-ref node "data"))
    (else (node-name node))))
