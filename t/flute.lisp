(in-package :cl-user)
(defpackage flute.test
  (:use :cl :flute))
(in-package :flute.test)

(define-element clock (id size)
  (div :id id
       (h1 "clock")
       (img "blabal" :size size)
       children))
