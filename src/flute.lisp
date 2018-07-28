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
  ((expand-to :initarg :expander
              :accessor user-element-expander)))

(defun make-builtin-element (&key tag attrs children)
  (make-instance 'builtin-element :tag tag :attrs attrs
                 :children (escape-children children)))

(defun make-builtin-element-with-prefix (&key tag attrs children prefix)
  (make-instance 'builtin-element-with-prefix :tag tag :attrs attrs :prefix prefix
                 :children (escape-children children)))

(defun make-user-element (&rest args &key tag attrs children expander)
  (make-instance 'user-element :tag tag :attrs attrs :expander expander
                 :children (escape-children children)))

(defmethod user-element-expand-to ((element user-element))
  (funcall (user-element-expander element)
           (element-tag element)
           (element-attrs element)
           (element-children element)))

(defstruct (attrs (:constructor %make-attrs))
  alist)

(defvar *escape-html* :utf8
  "Specify the escape option when generate html, can be :UTF8, :ASCII, :ATTR or NIL.
If :UTF8, escape only #\<, #\> and #\& in body, and \" in attribute keys. #\' will
in attribute keys will not be escaped since flute will always use double quote for
attribute keys.
If :ASCII, besides what escaped in :UTF8, also escape all non-ascii characters.
If :ATTR, only #\" in attribute values will be escaped.
If NIL, nothing is escaped and programmer is responsible to escape elements properly.
When given :ASCII and :ATTR, it's possible to insert html text as a children, e.g.
(div :id \"container\" \"Some <b>text</b>\")")

(defun make-attrs (&key alist)
  (if *escape-html*
      (%make-attrs :alist (escape-attrs-alist alist))
      (%make-attrs :alist alist)))

(defmethod (setf attr) (value (attrs attrs) key)
  (setf (aget (attrs-alist attrs) key) value))

(defmethod delete-attr ((attrs attrs) key)
  (delete-from-alistf (attrs-alist attrs) key))

(defmethod attr ((attrs attrs) key)
  (aget (attrs-alist attrs) key))

(defmethod (setf attr) (value (element element) key)
  (setf (attr (element-attrs element) key) value))

(defmethod delete-attr ((element element) key)
  (delete-attr (element-attrs element) key))

(defmethod attr ((element element) key)
  (attr (element-attrs element) key))

(defun html (&rest attrs-and-children)
  (multiple-value-bind (attrs children)
      (split-attrs-and-children attrs-and-children)
    (make-builtin-element-with-prefix :tag "html" :attrs attrs
                                      :children children
                                      :prefix "<!DOCTYPE html>")))

(defmacro define-builtin-element (element-name)
  `(defun ,element-name (&rest attrs-and-children)
     (multiple-value-bind (attrs children)
         (split-attrs-and-children attrs-and-children)
       (make-builtin-element :tag (string-downcase (mkstr ',element-name))
                             :attrs attrs :children children))))

(defmacro define-and-export-builtin-elements ()
  `(progn
     ,@(mapcan (lambda (e)
                 (list `(define-builtin-element ,e)
                       `(export ',e)))
               '#.*builtin-elements-list*)))

(define-and-export-builtin-elements)

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
       (let ((,g!element
              (make-user-element :tag (string-downcase ',name) :attrs ,g!attrs
                                 :children ,g!children)))
         (setf (user-element-expander ,g!element)
               (lambda (tag attrs children)
                 (declare (ignorable tag attrs children))
                 (let ,(mapcar (lambda (arg)
                                 (list arg `(attr attrs (make-keyword ',arg))))
                               args)
                   (progn ,@body))))
         ,g!element))))

(defvar *expand-user-element* t)

(defmethod print-object ((element user-element) stream)
  (if *expand-user-element*
      (print-object (user-element-expand-to element) stream)
      (call-next-method)))

(defmacro h (&body body)
  `(progn
     ,@(tree-leaves
        body
        (and (symbolp x) (not (keywordp x)) (gethash (collect-name-as-keyword x) *builtin-elements*))
        (find-symbol (string (collect-name-as-keyword x)) :flute))))

(defmethod element-string ((element element))
  (with-output-to-string (s)
    (write element :stream s)))

(defmethod elem-str ((element element))
  (with-output-to-string (s)
    (write element :stream s :pretty nil)))
