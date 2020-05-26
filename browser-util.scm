(load "mini-framework.scm")

;;;; Console

;; To call the browser function 'console.dir', define a Scheme function:
(define (console-dir obj)
  (js-call% "console.dir" obj))

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

;; Example HTTP request:
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
