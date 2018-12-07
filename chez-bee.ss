#!/usr/bin/scheme --script

#;(import (rnrs base))

;;; display license
(define display-license
  (lambda ()
    (display "MIT License") (newline)
    (display "Copyright (c) 2018 Z.Riemann") (newline)
    (display "https://github.com/ZRiemann/chez-bee") (newline)
    (newline)))

(display-license)

;;; TODO: set source file path, build path
;;; TODO: get source file extension
;;; TODO: compile each source file
;;; TODO: make distribution
