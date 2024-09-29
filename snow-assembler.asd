(asdf:defsystem "snow-assembler"
  :description "Draw a fractal"

  :author "mzan@dokmelody.org"
  :license  "LGPL-3.0-or-later"
  :depends-on (
     "alexandria"
     "trivial-types"
     "defstar"
     "iterate"
     "str"
     "let-plus"
     "array-operations"
     "sdl2"
     "cl-opengl"
     "cffi"
     "trivial-benchmark"
     "random-state")

  :components ((:file "snow-assembler")))
