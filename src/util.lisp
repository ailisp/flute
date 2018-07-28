(in-package :flute)

(defun plist-alist (plist)
  (loop for (k v) on plist by #'cddr
     collect (cons k v)))

(defun alist-plist* (alist)
  (mapcan (lambda (kv)
            (list (string-downcase (car kv))
                  (cdr kv)))
          alist))

(defstruct !expanded list)

(defun tree-leaves%% (tree test result)
  (if tree
      (if (listp tree)
          (let ((car-result (tree-leaves%% (car tree) test result))
                (cdr-result (tree-leaves%% (cdr tree) test result)))
            (if (!expanded-p car-result)
             (append (!expanded-list car-result) cdr-result)
             (cons car-result cdr-result)))
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
    ((and (vectorp (first attrs-and-children))
          (keywordp (aref (first attrs-and-children) 0)))
     (append-inline-attrs attrs-and-children))
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

(defun append-inline-attrs (attrs-and-children)
  (let* ((inline-attrs (coerce (first attrs-and-children) 'list))
         (id (getf inline-attrs :id))
         (class (getf inline-attrs :class)))
    (multiple-value-bind (attrs children)
        (split-attrs-and-children (rest attrs-and-children))
      (when (and id (not (attr attrs :id)))
        (setf (attr attrs :id) id))
      (when class
        (if-let (other-class (attr attrs :class))
          (setf (attr attrs :class) (format nil "~a ~a" class other-class))
          (setf (attr attrs :class) class)))
      (values attrs children))))

(defun collect-until-dot-or-sharp (string)
  (let ((pos (position-if (lambda (c) (or (char= c #\.) (char= c #\#))) string)))
    (if pos
        (cons (subseq string 0 pos) (subseq string pos))
        (cons string ""))))

(defun collect-name-as-keyword (symbol)
  (make-keyword (car (collect-until-dot-or-sharp (string symbol)))))

(defun collect-id-and-class (symbol)
  (let (name id class next-is)
    (do ((current-and-remains (collect-until-dot-or-sharp (string-downcase (string symbol)))
                              (collect-until-dot-or-sharp (cdr current-and-remains))))
        ((string= "" (car current-and-remains))
         (values name id (when class (format nil "~{~a~^ ~}" (nreverse class)))))
      (case next-is
        (:id (setf id (car current-and-remains)))
        (:class (push (car current-and-remains) class))
        (otherwise (setf name (car current-and-remains))))
      (unless (string= "" (cdr current-and-remains))
        (setf next-is (ecase (aref (cdr current-and-remains) 0)
                        (#\# :id)
                        (#\. :class))
              (cdr current-and-remains) (subseq (cdr current-and-remains) 1))))))
