
(use-modules
  ((guix licenses) #:prefix license:)
  (guix packages)
  (guix download)
  (guix gexp)
  (guix git-download)
  (guix build-system asdf)
  (guix build-system gnu)
  (guix utils)
  (gnu packages)
  (gnu packages bash)
  (gnu packages admin)
  (gnu packages autotools)
  (gnu packages base)
  (gnu packages lisp)
  (gnu packages lisp-xyz)
  (gnu packages commencement))

(define %source-dir (dirname (current-filename)))

(package
    (name "snow-assembler")
    (version "0.1")
    (source (local-file %source-dir #:recursive? #t))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     (list
        sbcl
        sbcl-slynk
        sbcl-agnostic-lizard

        sbcl-defstar
        sbcl-trivia
        sbcl-alexandria
        sbcl-trivial-types
        sbcl-cl-str
        sbcl-parse-float
        sbcl-iterate
        sbcl-let-plus
        sbcl-array-operations
        sbcl-sdl2
        sbcl-trivial-benchmark
        sbcl-random-state))
    (outputs '("out" "lib"))
    (synopsis "Generate a fractal image")
    (description
     "Generate a fractal image.")
    (home-page "")
    (license license:lgpl3+))
