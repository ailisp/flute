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

(defun make-builtin-element (&key tag attrs children)
  (make-instance 'builtin-element :tag tag :attrs attrs
                 :children (escape-children children)))

(defun make-builtin-element-with-prefix (&key tag attrs children prefix)
  (make-instance 'builtin-element-with-prefix :tag tag :attrs attrs :prefix prefix
                 :children (escape-children children)))

(defun make-user-element (&rest args &key tag attrs children expand-to)
  (make-instance 'user-element :tag tag :attrs attrs :expand-to expand-to
                 :children (escape-children children)))

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

(defmacro h (&body body)
  `(progn
     ,@(tree-leaves
        body
        (and (symbolp x) (not (keywordp x)) (gethash (make-keyword x) *builtin-elements*))
        (find-symbol (string x) :flute))))
