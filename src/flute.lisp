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
   ;; all html5 elements, e.g. div, nav, media, export in code
   ;; except <time> and <map> conflicts with cl symbol, are defined and exported as |time|, |map|
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
(in-package :flute)

(defclass element ()
  ((tag :initarg :tag
        :accessor element-tag)
   (attrs :initarg :attrs
          :accessor element-attrs)
   (children :initarg :children
             :accessor element-children)))

(defclass builtin-element (element) ())

(defclass builtin-element-with-prefix (builtin-element)
  ((prefix :initarg :prefix
           :accessor element-prefix)))

(defclass user-element (element)
  ((expand-to :initarg :expand-to
              :accessor user-element-expand-to)))

(defun make-builtin-element (&rest args &key tag attrs children)
  (apply #'make-instance 'builtin-element args))

(defun make-builtin-element-with-prefix (&rest args &key tag attrs children prefix)
  (apply #'make-instance 'builtin-element-with-prefix args))

(defun make-user-element (&rest args &key tag attrs children expand-to)
  (apply #'make-instance 'user-element args))

(defstruct attrs alist)

(defmethod (setf attr) (value (attrs attrs) key)
  (setf (aget (attrs-alist) key) value))

(defmethod delete-attr ((attrs attrs) key)
  (delete-from-alistf (attrs-alist attrs) key))

(defmethod attr ((attrs attrs) key)
  (aget (attrs-alist attrs) key))

(defmethod (setf attr) (value (element element) key)
  (setf (attr (element-attrs element) key) value))

(defmethod delete-attr ((element element) key)
  (delete-attr (element-attrs element) key))

(defmethod attr ((element element) key)
  (attr (element-attrs element)))

(defun split-attrs-and-children (attrs-and-children)
  (cond
    ((attrs-p (first attrs-and-children))
     (values (first attrs-and-children) (flatten (rest attrs-and-children))))
    ((alistp (first attrs-and-children))
     (values (make-attrs :alist (first attrs-and-children))
             (flatten (rest attrs-and-children))))
    ((listp (first attrs-and-children))
     (values (make-attrs :alist (plist-alist (first attrs-and-children)))
             (flatten (rest attrs-and-children))))
    ((hash-table-p (first attrs-and-children))
     (values (make-attrs :alist (hash-alist (first attrs-and-children)))
             (flatten (rest attrs-and-children))))
    ((keywordp (first attrs-and-children))
     (loop for thing on attrs-and-children by #'cddr
        for (k v) = thing
        when (and (keywordp k) v)
          collect (cons k v) into attrs
        when (not (keywordp k))
          return (values (make-attrs :alist attrs) (flatten thing))
        finally (return (values (make-attrs :alist attrs) nil))))
    (t
     (values (make-attrs :alist nil) (flatten attrs-and-children)))))

(defun plist-alist (plist)
  (loop for (k v) on plist by #'cddr
     collect (cons k v)))

(defun alist-plist* (alist)
  (mapcan (lambda (kv)
            (list (string-downcase (car kv))
                  (cdr kv)))
          alist))

(defvar *builtin-elements* (make-hash-table))

(defun html (&rest attrs-and-children)
  (multiple-value-bind (attrs children)
      (split-attrs-and-children attrs-and-children)
    (make-builtin-element-with-prefix :tag "html" :attrs attrs
                                      :children children
                                      :prefix "<!DOCTYPE html>")))
(setf (gethash :html *builtin-elements*) t)

(defmacro define-builtin-element (element-name)
  `(defun ,element-name (&rest attrs-and-children)
     (multiple-value-bind (attrs children)
         (split-attrs-and-children attrs-and-children)
       (make-builtin-element :tag (string-downcase (mkstr ',element-name))
                             :attrs attrs :children children))))

(defmacro define-and-export-builtin-elements (&rest element-names)
  `(progn
     ,@(mapcan (lambda (e)
                 (list `(define-builtin-element ,e)
                       `(setf (gethash (make-keyword ',e) *builtin-elements*) t)
                       `(export ',e)))
               element-names)))

(define-and-export-builtin-elements
    a abbr address area article aside audio b base bdi bdo blockquote
    body br button canvas caption cite code col colgroup data datalist
    dd del details dfn dialog div dl dt em embed fieldset figcaption
    figure footer form h1 h2 h3 h4 h5 h6 head header hr i iframe
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
  (if (element-children element)
      (format stream (if (rest (element-children element))
                         "~@<<~a~a>~2I~:@_~<~@{~a~^~:@_~}~:>~0I~:@_</~a>~:>"
                         "~@<<~a~a>~2I~:_~<~a~:>~0I~:_</~a>~:>")
              (element-tag element)
              (element-attrs element)
              (element-children element)
              (element-tag element))
      (format stream "<~a~a>" (element-tag element) (element-attrs element))))

(defmethod print-object ((element builtin-element-with-prefix) stream)
  (format stream "~a~%" (element-prefix element))
  (call-next-method))

(defmacro! define-element (name (&rest args) &body body)
  `(defun ,name (&rest ,g!attrs-and-children)
     (multiple-value-bind (,g!attrs ,g!children)
         (split-attrs-and-children ,g!attrs-and-children)
       (let ,(mapcar (lambda (arg)
                       (list arg `(cdr (assoc (make-keyword ',arg) (attrs-alist ,g!attrs)))))
                     args)
         (make-user-element :tag (string-downcase ',name) :attrs ,g!attrs
                            :children ,g!children
                            :expand-to
                            (lambda (tag attrs children)
                              (declare (ignorable tag attrs children))
                              (progn ,@body)))))))

(defvar *expand-user-element* t)

(defmethod print-object ((element user-element) stream)
  (if *expand-user-element*
      (print-object (funcall (user-element-expand-to element)
                             (element-tag element)
                             (element-attrs element)
                             (element-children element))
                    stream)
      (call-next-method)))

(defun tree-leaves%% (tree test result)
  (if tree
    (if (listp tree)
      (cons
        (tree-leaves%% (car tree) test result)
        (tree-leaves%% (cdr tree) test result))
      (if (funcall test tree)
        (funcall result tree)
        tree))))

(defmacro tree-leaves (tree test result)
  `(tree-leaves%%
     ,tree
     (lambda (x)
       (declare (ignorable x))
       ,test)
     (lambda (x)
       (declare (ignorable x))
       ,result)))

(defmacro h (&body body)
  `(progn
     ,@(tree-leaves
        body
        (and (symbolp x) (not (keywordp x)) (gethash (make-keyword x) *builtin-elements*))
        (find-symbol (string x) :flute))))
