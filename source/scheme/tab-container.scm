; Copyright (C) 2021 Daniel Koning
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

(load "scheme/browser-util.scm")
;; Should config.scm be loaded as well in every new Scheme file, in case it
;; eventually comes to include something of universal import (so to speak)?


;;;; Extensions to the framework

(define (getelems selector)
  ;; Oddly, this works even for the value #f that is returned when getelem finds
  ;; nothing. That doesn't seem like the Scheme way to me, but what do I know.
  (js-array->list (getelem selector)))


;;;; Convenience functions

;; Write to the designated box.
(define (message str)
  (set-inner-html! (getelem1 "#message")
                   (string-append "<span class='message'>" str "</span>")))

(define (date-as-string date-obj)
  (js-invoke date-obj "toString"))

(define (current-date-as-string)
  ;; Maybe a format different from the default would be nicer.
  (date-as-string (js-new "Date")))


;;;; DOM communication

;; To clarify: a "tab" in vaadin is only the labeled button-like element which
;; is clicked to trigger something else.
(define (all-tabs)
  (getelems "#tab-container vaadin-tabs vaadin-tab"))

;; In this case, what it triggers is the display of a so-called "subpage" in the
;; div beneath the tab bar.
(define (all-subpages)
  (getelems "#subpages p"))

(define (selected-tab)
  (or
   (find (lambda (tab)
          (equal? (element-read-attribute tab "aria-selected") "true"))
         (all-tabs))
   (console-error "Somehow there's no selected tab. How?")))


;;;; Internal logic associating each tab with the subpage it's meant to reveal
;;;; when the user clicks it

(define tab-subpage-pairs (list
               (cons (getelem1 "#starter-tab")
                     (getelem1 "#starter-subpage"))))

(define (new-tab-and-subpage key)
  (let* ((date (current-date-as-string))
         (tab (element-new
               (list "vaadin-tab" key)))
         (subpage (element-new
                (list "p" (format "Created at ~a" date)))))
    (push! (cons tab subpage) tab-subpage-pairs)
    (list tab subpage)))

(define (subpage-for-tab tab)
  (let ((pair (assq tab tab-subpage-pairs)))
    (if pair
        (cdr pair)
        (console-error
         "Somehow the tab-subpage association list got corrupted."))))


;;;; Event handlers

;; Add a new tab; assign it a default name based on the order in which it was
;; added.
(define add-tab
  ;; This function doesn't really HAVE to close around a lexical variable, but
  ;; that seems a good way of illustrating the concept in a demo.
  (let ((tab-count 1))
    (lambda ()
      (set! tab-count (+ 1 tab-count))

      (let*
          ((tab-bar (getelem1 "#tab-container vaadin-tabs"))
           (subpage-holder (getelem1 "#subpages"))
           (key (format "~a" tab-count))
           (tab-and-subpage (new-tab-and-subpage key))
           ;; We could really use some kind of destructuring macro.
           (tab (first tab-and-subpage))
           (subpage (second tab-and-subpage)))

        (append-child tab tab-bar)

        (element-hide! subpage)
        (append-child subpage subpage-holder)

        (message (format "Congratulations. You just created tab ~a." key))))))


;; Determine which is the active tab; display the associated subpage, and only
;; that subpage (clearing any that was already showing).
(define (update-displayed-subpage)
  (map element-hide! (all-subpages))

  (let* ((tab (selected-tab))
         (subpage (subpage-for-tab tab)))
    (element-show! subpage)

    (message "Good job. You just clicked the tab bar.")))


;;;; The top-level loop registering handlers

(define (run)
  (message "Click the button to add a tab.")

  ;; This might as well just go on forever.
  (while #t
    (with-handlers ((click-handler "#add")
                    (click-handler "#tab-container"))

      (let* ((input (get-input))
             (jquery-event (second input))
             (target (js-ref jquery-event "currentTarget"))
             (target-id (js-ref target "id")))

        (case target-id
          (("add") (add-tab))
          ;; Really, this one should only fire when the user clicks an actual
          ;; tab, rather than just the background behind or around them; but I
          ;; haven't come up with an elegant way to do that yet.
          (("tab-container") (update-displayed-subpage)))))))
