;;;; package.lisp

(defpackage #:forthlike
  (:use #:cl #:split-sequence)
  (:export #:repl #:forthlike-eval #:def #:env))

