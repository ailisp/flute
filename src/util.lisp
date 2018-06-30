(in-package :flute)

(defun plist-alist (plist)
  (loop for (k v) on plist by #'cddr
     collect (cons k v)))

(defun alist-plist* (alist)
  (mapcan (lambda (kv)
            (list (string-downcase (car kv))
                  (cdr kv)))
          alist))

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

(defun utf8-html-escape-char-p (char)
  (find char "<>&"))

(defun ascii-html-escape-char-p (char)
  (or (utf8-html-escape-char-p char)
      (> (char-code char) 127)))

(defun attr-value-escape-char-p (char)
  (eql char #\"))

(defun escape-char (char)
  (case char
    (#\< "&lt;")
    (#\> "&gt;")
    (#\& "&amp;")
    (#\' "&#039;")
    (#\" "&quot;")
    (t (format nil "&#~d;" (char-code char)))))

(defun escape-string (string &optional (test #'utf8-html-escape-char-p))
  (if (stringp string)
      (with-output-to-string (s)
        (loop
           for c across string
           do (write (if (funcall test c) (escape-char c) c) :stream s :escape nil)))
      string))

(defun escape-attrs-alist (alist)
  (mapcar (lambda (kv)
            (cons (car kv)
                  (escape-string (cdr kv) #'attr-value-escape-char-p)))
          alist))

(defun escape-children (children)
  (mapcar (lambda (child)
            (if (stringp child)
                (case *escape-html*
                  (:utf8 (escape-string child))
                  (:ascii (escape-string child #'ascii-html-escape-char-p))
                  (otherwise child))
                child))
          children))

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
