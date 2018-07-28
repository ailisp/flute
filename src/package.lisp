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
                :make-keyword
                :if-let)
  (:export
   ;;; builtin HTML elements
   ;;; all html5 elements, e.g. div, nav, media, export in code except
   ;;; <time> and <map> conflicts with cl symbol, are defined and
   ;;; exported as |time|, |map|
   :html

   ;;; user defined elements
   :define-element
   :*expand-user-element*
   ;; for reference tag name, attributes and children elements in user
   ;; element definition
   :tag
   :children
   :attrs

   ;;; attribute accessing utilility
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
   :attr-value-escape-char-p

   ;;; helper for generate html string
   :element-string
   :elem-str))
