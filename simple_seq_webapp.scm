; Copyright (C) 2020 Thomas Elam
;
; This file is part of web-call.cc.
;
; web-call.cc is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; web-call.cc is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with web-call.cc.  If not, see <https://www.gnu.org/licenses/>.

(load "mini-framework.scm")

;; Everything in this file below this sentence
;; was programmed by Alexander Sukhoverkhov (naryl.pandora@gmail.com).

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
