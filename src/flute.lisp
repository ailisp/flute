(in-package :cl-user)
(defpackage flute
  (:use :cl)
  (:import-from :assoc-utils
                :alist
                :alistp
                :hash-alist
                :alist-plist)
  (:import-from :let-over-lambda
                :defmacro!
                :mkstr)
  (:export
   ;; all html5 elements, e.g. div, nav, media, export in code
   ;; except <time> and <map> conflicts with cl symbol, are defined and exported as |time|, |map|

   ))
(in-package :flute)

(defstruct element tag attrs children)

(defstruct attrs alist)

(defun split-attrs-and-chilren (attrs-and-children)
  (cond
    ((attrs-p (first attrs-and-children))
     (values (first attrs-and-children) (rest attrs-and-children)))
    ((alistp (first attrs-and-children))
     (values (make-attrs :alist (first attrs-and-children))
             (rest attrs-and-children)))
    ((listp (first attrs-and-children))
     (values (make-attrs :alist (plist-alist (first attrs-and-children)))
             (rest attrs-and-children)))
    ((hash-table-p (first attrs-and-children))
     (values (make-attrs :alist (hash-alist (first attrs-and-children)))
             (rest attrs-and-children)))
    ((keywordp (first attrs-and-children))
     (loop for thing on attrs-and-children by #'cddr
        for (k v) = thing
        when (keywordp k)
          collect (cons k v) into attrs
        when (not (keywordp k))
          return (values (make-attrs :alist attrs) thing)
        finally (return (values (make-attrs :alist attrs) nil))))
    (t
     (values (make-attrs :alist nil) attrs-and-children))))

(defun plist-alist (plist)
  (loop for (k v) on plist by #'cddr
     collect (cons k v)))

(defun alist-plist* (alist)
  (mapcan (lambda (kv)
            (list (string-downcase (car kv))
                  (cdr kv)))
          alist))

(defmacro define-builtin-element (element-name)
  `(defun ,element-name (&rest attrs-and-children)
     (multiple-value-bind (attrs children) (split-attrs-and-chilren attrs-and-children)
       (make-element :tag (string-downcase (mkstr ',element-name)) :attrs attrs :children children))))

(defmacro define-and-export-builtin-elements (&rest element-names)
  `(progn
     ,@(mapcan (lambda (e)
                 (list `(define-builtin-element ,e)
                       `(export ',e)))
               element-names)))

(define-builtin-elements
    a abbr address area article aside audio b base bdi bdo blockquote
    body br button canvas caption cite code col colgroup data datalist
    dd del details dfn dialog div dl dt em embed fieldset figcaption
    figure footer form h1 h2 h3 h4 h5 h6 head header hr html i iframe
    img input ins kbd label legend li link main |map| mark meta meter nav
    noscript object ol optgroup option output p param picture pre progress
    q rp rt ruby s samp script section select small source span strong
    style sub summary sup svg table tbody td template textarea tfoot th
    thead |time| title tr track u ul var video wbr)

(defmethod print-object ((attrs attrs) stream)
  (if (attrs-alist attrs)
      (format stream " ~{~a=~s~^ ~}" (alist-plist* (attrs-alist attrs)))
      (format stream "")))

(defmethod print-object ((element element) stream)
  (format stream "<~a~a>" (element-tag element) (element-attrs element))
  (when (element-children element)
    (format stream "~%~<~2I~@{~a~^~:@_~}~:>~%" (element-children element)))
  (format stream "</~a>~%" (element-tag element)))

(defmethod print-object ((element element) stream)
  (if (element-children element)
      (format stream (if (rest (element-children element))
                         "~@<<~a~a>~2I~:@_~<~@{~a~^~:@_~}~:>~0I~:@_</~a>~:>"
                         "~@<<~a~a>~2I~:_~<~a~:>~0I~:_</~a>~:>")
              (element-tag element)
              (element-attrs element)
              (element-children element)
              (element-tag element))
      (format stream "<~a~a>" (element-tag element) (element-attrs element))))

;; (defmacro! define-element (name (&rest args) &body body)
;;   `(defun ,name (&rest g!attrs-and-children)))
