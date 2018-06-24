(defsystem flute-test
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :depends-on (:flute)
  :components ((:module "t"
                :serial t
                :components
                ((:file "flute")))))
