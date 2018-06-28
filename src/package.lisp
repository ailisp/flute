(in-package :cl-user)
(defpackage flute
  (:use :cl)
  (:import-from :assoc-utils
                :alist
                :alistp
                :hash-alist
                :aget
                :delete-from-alistf)
  (:import-from :let-over-lambda
                :defmacro!
                :mkstr
                :flatten)
  (:import-from :alexandria
                :make-keyword)
  (:export
   ;; all html5 elements, e.g. div, nav, media, export in code except
   ;; <time> and <map> conflicts with cl symbol, are defined and
   ;; exported as |time|, |map|
   :define-element
   :attrs
   :attrs-alist
   :make-attrs
   :copy-attrs
   :html
   :element-tag
   :element-attrs
   :element-children
   :user-element-expand-to
   :*expand-user-element*
   :h))
