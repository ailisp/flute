(in-package :flute)

(defgeneric element-tag (element)
  (:documentation "=> string
Get the tag name. For example <html>'s `ELEMENT-TAG` is \"html\""))

(defgeneric (setf element-tag) (tag element)
  (:documentation "Set the tag name given a string"))

(defgeneric element-attrs (element)
  (:documentation "=> attrs
Get the element attrs object."))

(defgeneric (setf element-attrs) (attrs element)
  (:documentation "Set the element attrs object. `ATTRS` must be an attrs object"))

(defgeneric element-children (element)
  (:documentation "=> list
Get element children."))

(defgeneric (setf element-children) (children element)
  (:documentation "Set element children. `CHILDR#EN` must given a flatten list,
Each element of list must be either element object or or string."))

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
  "=> element
Get what this `ELEMENT` expands to.  Returns the root element after it expands."
  (funcall (user-element-expander element)
           (element-tag element)
           (element-attrs element)
           (element-children element)))

(defstruct (attrs (:constructor %make-attrs))
  alist)
(setf (documentation 'attrs-alist 'function)
      "Get the attrs object in alist format.")
(setf (documentation '(setf attrs-alist) 'function)
      "Set the attrs object has attributes in alist format. The key in alist must be
a keyword, and value in alist must be string.")
(setf (documentation 'copy-attrs 'function)
      "Make a copy and return the copy of attrs object.")

(defvar *escape-html* :utf8
  "Specify the escape option when generate html, can be `:UTF8`, `:ASCII`, `:ATTR` or `NIL`.
If `:UTF8`, escape only `#\<`, `#\>` and `#\&` in body, and `\"` in attribute keys. `#\'` will
in attribute keys will not be escaped since flute will always use double quote for attribute
keys.
If `:ASCII`, besides what escaped in `:UTF8`, also escape all non-ascii characters.
If `:ATTR`, only `#\"` in attribute values will be escaped.
If `NIL`, nothing is escaped and programmer is responsible to escape elements properly.
When given `:ASCII` and `:ATTR`, it's possible to insert html text as a children, e.g.
`(div :id \"container\" \"Some <b>text</b>\")`")

(defun make-attrs (&key alist)
  "Create a attrs aoject, given an alist of `(:attr . \"attr-value\")` pair.
Attribute values (cdr of each element in alist) will be escaped if
`*ESCAPE-HTML*` is not `NIL`."
  (if *escape-html*
      (%make-attrs :alist (escape-attrs-alist alist))
      (%make-attrs :alist alist)))

(defgeneric attr (attrs key)
  (:documentation "Get the attribute value of given `KEY`, `KEY` should be an keyword.
If `KEY` doesn't exist, return NIL."))

(defgeneric (setf attr) (value attrs key)
  (:documentation "Set the attribute value of given `KEY` with `VALUE`. `VALUE` must be
a string. If `KEY` doesn't exist, will insert the key value pair."))

(defgeneric delete-attr (attrs key)
  (:documentation "Delete the attribute key value pair from attrs or wlement's element-attrs,
will ignore if `KEY` doesn't exist."))

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

(defvar *builtin-elements* (make-hash-table))

(defun html (&rest attrs-and-children)
  "Available HTML5 element has same usage as this one:

    a abbr address area article aside audio b base bdi bdo blockquote
    body br button canvas caption cite code col colgroup data datalist
    dd del details dfn dialog div dl dt em embed fieldset figcaption
    figure footer form h1 h2 h3 h4 h5 h6 head header hr i iframe html
    img input ins kbd label legend li link main |map| mark meta meter nav
    noscript object ol optgroup option output p param picture pre progress
    q rp rt ruby s samp script section select small source span strong
    style sub summary sup svg table tbody td template textarea tfoot th
    thead |time| title tr track u ul var video wbr

Take `A` as example, it will create and return an `<a>` element object.
`ATTRS-AND-CHILDREN` can be the following:

1. Not specified, then it create an empty `<a>`:
```lisp
(a)
```

2. Attributes can be alist, plist or ATTRS object. The following creates:
`<a id=\"aa\" customer-attr=\"bb\">`:
```lisp
(a :id \"aa\" :customer-attr \"bb\")
(a '(:id \"aa\" :customer-attr \"bb\"))
(a '((:id . \"aa\") (:customer-attr . \"bb\")))
```

And if we have the above created element store in variable a1, we can copy it by:
```lisp
(a (element-attrs a1)) ; to share the same attrs object with a1
(a (copy-attrs (element-attrs a1)))
```

3. The remaining argument will be recognized as the children of this element. Each
child can be:
  a. string;
  b. element, builtin or user defined;
  c. list of a, b and c. Can also be NIL.
All children will be flattened as if they're given inline.
And children can be combined with all kind of attributes. Note if first children is
a list then attribute can't be omit. If there's no attributes for this element,
using `()` as attributes.
Examples:
```lisp
(a :id \"aa\" :customer-attr \"bb\"
  \"Some children\"
  (div '(:id \"an element children\"))
  ; list of any depth containing elements and texts, will be flattened
  (list a1 a2 (a '((:id . \"aaa\")) \"some text\")
        (list (h1 \"aaa\")))
  \"some other text\")
```
"
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
  "Define a user element with `NAME` as its tag name and function
`NAME`. After `DEFINE-ELEMENT`, a function of `NAME` in current package
is defined. `ARGS` specified the possible keyword `ARGS` it can take as
it's `ATTRS`. You can either use these `ARGS` as Lisp arguments in the
`BODY` of its definition and plug in them to the `BODY` it expand to.
You can use `FLUTE:CHILDREN` to get or set it's children that you give
when call function `NAME`, `FLUTE:ATTRS` to get or set it's attributes
and `FLUTE:TAG` to get or set it's tag name.
"
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

(defvar *expand-user-element* t
  " Bind this variable to specify whether the user elements are print in
a high level (NIL), or expand to HTML elements (T). T by default.")

(defmethod print-object ((element user-element) stream)
  (if *expand-user-element*
      (print-object (user-element-expand-to element) stream)
      (call-next-method)))

(defun html-element-p (x)
  (and (symbolp x) (not (keywordp x)) (gethash (collect-name-as-keyword x) *builtin-elements*)))

(defmacro h (&body body)
  "Like a `progn`, except it will replace all html tag symbol with the same name one
in flute package, so you don't need to import all of them. As an alternative you
can import all or part of html element functions in flute package to use them
without `H` macro"
  `(progn
     ,@(tree-leaves
        body
        (html-element-p x)
        (multiple-value-bind (name id class) (collect-id-and-class x)
          (if (or id class)
              (make-!expanded :list (list (find-symbol (string-upcase name) :flute)
                                          (coerce (append (when id (list :id id))
                                                          (when class (list :class class)))
                                                  'vector)))
              (find-symbol (string-upcase name) :flute))))))

;;; Experimental
;; (when (find :illusion *features*)
;;   (illusion:set-paren-reader
;;    :flute
;;    #'html-element-p
;;    (lambda (stream indicator)
;;      (multiple-value-bind (name id class) (collect-id-and-class indicator)
;;        (if (or id class)
;;            (list* (find-symbol (string-upcase name) :flute)
;;                   (coerce (append (when id (list :id))
;;                                  (when class (list :class class)))
;;                           'vector)
;;                   (illusion:cl-read-list stream))
;;            (cons (find-symbol (string-upcase name) :flute)
;;                  (illusion:cl-read-list stream)))))))

(defmethod element-string ((element element))
  "=> string
Return human readable, indented HTML string for `ELEMENT`."
  (with-output-to-string (s)
    (write element :stream s)))

(defmethod elem-str ((element element))
  "=> string
Return minify HTML string for `ELEMENT`."
  (with-output-to-string (s)
    (write element :stream s :pretty nil)))
