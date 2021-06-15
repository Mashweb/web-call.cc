---
description: >-
  To bring all user and Ajax interaction under the control of a single,
  sequential process, the main thread of execution must wait for the execution
  of at least one JavaScript event handler.
---

# Synchronization of JavaScript Events

Here is a simple program that illustrates the funnelling of all asynchronous JavaScript events into a single queue:

```text
(load "mini-framework.scm")
(define (test)
  (with-handlers ((click-handler "#div1")
                  (click-handler "#div2")
                  (keydown-handler "#button1")
                  (keydown-handler "#button2")
                  (timeout-handler test-timeout 10000))
                 (display (get-input))
                 (display (get-input))
                 (display (get-input))
                 (display (get-input))
                 (display (get-input)))
  (display "Test finished."))
  
(reset (test))
```

The program is described at [https://doc.mashweb.club/experiments/seq\_webapp\_biwascheme/](https://doc.mashweb.club/experiments/seq_webapp_biwascheme/) .

