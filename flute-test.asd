(defsystem flute-test
  :author "Bo Yao <icerove@gmail.com>"
  :license "MIT"
  :depends-on (:flute :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "flute")))))
