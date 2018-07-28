(defsystem flute
  :author "Bo Yao <icerove@gmail.com>"
  :license "MIT"
  :version "0.1"
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "util")
                 (:file "element-belongs-to")
                 (:file "flute"))))
  :description "A beautiful, easilly composable HTML5 generation library"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op flute-test)))
  :depends-on (:assoc-utils
               :let-over-lambda))
