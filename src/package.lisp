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

   ;;; all html5 elements, e.g. div, nav, media, export in code except
   ;;; <time> and <map> conflicts with cl symbol, are defined and
   ;;; exported as |time|, |map|
   :html

   ;;; user define elements
   :define-element
   :*expand-user-element*

   ;;; attribute accessing utilility
   :attrs
   :attrs-alist
   :make-attrs
   :copy-attrs
   :attr
   :delete-attr

   ;;; element slots
   :element-tag
   :element-attrs
   :element-children
   :user-element-expand-to

   ;;; the h macro for avoiding import all builtin html element functions
   :h

   ;;; escape utility
   :*escape-html*
   :escape-string
   :utf8-html-escape-char-p
   :ascii-html-escape-char-p
   :attr-value-escape-char-p))
