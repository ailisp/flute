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
  (with-output-to-string (s)
    (loop
       for c across string
       do (write (if (funcall test c) (escape-char c) c) :stream s :escape nil))))

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
