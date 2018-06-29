(defsystem flute-test
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :depends-on (:flute :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "flute")))))
