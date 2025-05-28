(defsystem "ttl-cl"
  :description "The Little Learner in CL"
  :version     "0.1.0"
  :author      "Your Name <you@example.com>"
  :depends-on  (:mgl
                :mgl-gnuplot
                :fiveam
                :binding-arrows
                :serapeum)
  :serial t
  :components
  ((:file "tens")
   (:file "ch1")
   (:file "ch2")))
