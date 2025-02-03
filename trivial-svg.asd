(defsystem trivial-svg
  :author "April & May"
  :license "0BSD"
  :description "Pure Lisp SVG renderer."
  :depends-on (alexandria cl-ppcre plump serapeum vecto uiop)
  :components ((:file "trivial-svg")))
