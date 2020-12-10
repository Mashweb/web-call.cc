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

;; Function for appending a script or link to the head of the HTML document.
(define (add-script! script)
  (element-append-child! (getelem "head") (element-new script)))

;; Add support for web components to older browsers.
(add-script! '(script src "https://unpkg.com/@webcomponents/webcomponentsjs/webcomponents-loader.js"))

;; Load extra fonts.
(add-script! '(link href "https://fonts.googleapis.com/css?family=Roboto:300,400,500"
		    rel "stylesheet"))
(add-script! '(link href "https://fonts.googleapis.com/css?family=Material+Icons&display=block"
		    rel "stylesheet"))

;; Import the Material Web Components button.
(add-script!
 '(script
   type "text/javascript"
   "import('https://unpkg.com/@material/mwc-button/mwc-button.js?module');"
   ))

;; Function to make an mwc-button.
(define (make-mwc-button arg class)
  (if (number? arg)
      (define mwc-button (element-new `(mwc-button class "button" ,(number->string arg) raised "")))
      (define mwc-button (element-new `(mwc-button class "button" ,arg raised ""))))
  mwc-button)
