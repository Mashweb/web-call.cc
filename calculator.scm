(load "mini-framework.scm")
(load "web-components.scm")

;; Function for adding an mwc-button to the page.
(define (add-calculator-button label)
  (element-append-child! (getelem "#testarea2") (make-mwc-button label)))

(define (setup-buttons)
  (js-eval "$('#testarea2 mwc-button').remove()")

  (dotimes (i 9)
           (add-calculator-button (+ 1 i)))
  (add-calculator-button 0)

  (add-calculator-button "+")
  (add-calculator-button "-")
  (add-calculator-button "*")
  (add-calculator-button "/")
  (add-calculator-button "="))

;;;; New functional interface (no macros)

(define handlers
  '((click-handler ".button")))

(define (handle-keypress)
  (let ((btn (js-ref (second (get-input)) "target")))
    (display (js-ref btn "innerText"))))

;;(setup-buttons)
;;(reset (call-with-handlers handlers handle-keypress))

;;;; with-handlers is fine too

(define value1 0)
(define value2 0)
(define op #f)

(define (test)
  (with-handlers ((click-handler ".button"))
    (let* ((btn (js-ref (second (get-input)) "target"))
           (text (js-ref btn "innerText")))
      (case text
        (("+" "-" "*" "/")
         (when (not (= value2 0))
           (set! value1 value2))
         (set! value2 0)
         (set! op (string->symbol text)))
        (("=")
         (when op
           (set! value1 ((eval op) value1 value2))
           (set! value2 0)))
        (else
         (set! value2 (+ (* value2 10) (string->number text)))))
      (if op
          (format #t "~a ~a ~a~%" value1 (symbol->string op) value2)
          (format #t "~a~%" value2))))
  (test))

(setup-buttons)
(reset (test))
