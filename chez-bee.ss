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

;;; newer-source
;;; use case:
;;; source file is newer the object file, then need recompile
(define-syntax newer-source?
  (syntax-rules ()
    [(_ source obj)
     (if (file-exists? source)
         (if (file-exists? obj)
             (time>? (file-modification-time source) (file-modification-time obj))
             #t)
         #f)]))

;;; for-each-list
(define-syntax list-for-each
  (syntax-rules ()
    [(_ ls handler)
     (when (and (list? ls) (> (length ls) 0) (procedure? handler))
           (let visiter ([item (car ls)] [remain (cdr ls)])
             (handler item)
             (unless (null? remain)
                     (visiter (car remain) (cdr remain)))))]))

;;; file-tree-walk
(define-syntax file-tree-walk
  (syntax-rules ()
    [(_ path handler recursive?)
     (let ftw ([cur-path path])
       (cd cur-path)
       (call-with-values
           (lambda ()
             (partition file-directory? (directory-list cur-path)))
         (lambda (dirs files)
           (list-for-each files handler)
           (when recursive?
                 (set! dirs
                       (map (lambda (dir)
                              (string-append cur-path "/" dir))
                            dirs))
                 (list-for-each dirs ftw)))))]))

;;; check <fname> is Chez Scheme source file
;;;
;;; (chez-source? "src.ss") => (#t . "src.so")
;;; (chez-source? "not-src.xxx") => (#f . '())
(define-syntax chez-source?
  (syntax-rules ()
    [(_ fname)
     (call/cc
      (lambda (break)
        (list-for-each (library-extensions)
                       (lambda (extension)
                         (let ([ext (car extension)]
                               [len (string-length fname)]
                               [ext-len (string-length (car extension))])
                           (when (and (> len ext-len)
                                      (string=? ext (substring fname
                                                               (- len ext-len)
                                                               len)))
                                 (break (cons #t
                                              (string-append (substring fname 0 (- len ext-len))
                                                             (cdr extension))))))))
        (cons #f '())))]))

(define-syntax make-dir
  (syntax-rules ()
    [(_ path)
     (unless (file-exists? path)
             (let make([pos 1]
                       [len (string-length path)])
               (when (<= pos len)
                     (if (or (= pos len)
                             (eqv? (string-ref path (- pos 1)) (directory-separator)))
                         (begin
                           (unless (file-exists? (substring path 0 pos))
                                   (mkdir (substring path 0 pos)))
                           (make (+ pos 1) len))
                         ;; else
                         (make (+ pos 1) len)))))]))

(define main
  (lambda (src-path obj-path)
    (let* ([check-obj (lambda (src obj)
                        ;; check <src> is newer then <obj-file>
                        ;; ${src_path}/aaa/bbb/ccc.ss
                        ;; ${obj-path}/aaa/bbb/ccc.${ext}
                        (let* ([cur-dir (cd)]
                               [src-name (string-append cur-dir
                                                        (string (directory-separator))
                                                        src)]
                               [diff-path (substring cur-dir
                                                     (string-length src-path)
                                                     (string-length cur-dir))]
                               [obj-name (string-append obj-path
                                                        diff-path
                                                        (string (directory-separator))
                                                        obj)])
                          ;; make diff-path
                          (unless (string=? "" diff-path)
                                  (make-dir (string-append obj-path
                                                           (string (directory-separator))
                                                           diff-path)))
                          ;; (maybe-compile-file src-name obj-name)
                          ;; check newer source file
                          (if (newer-source? src-name obj-name)
                              (begin
                                (compile-file src-name obj-name)
                                obj-name)
                              '())))]
           [build-depedents (lambda (src) src)]
           [compile-file (lambda (fname)
                           (let* ([res (chez-source? fname)]
                                  [obj (cdr res)])
                             (when (car res)
                                   (let ([obj-file (check-obj fname obj)])
                                     (unless (null? obj-file)
                                             ;; check compile-program or compile-library
                                             ;; or compile-file

                                             (display obj-file) (newline))))))]
           [create-distribution-package (lambda ()
                                          (display "create distribution...\n"))])
      ;; compile files
      (file-tree-walk src-path compile-file #t)
      ;; create distribution package
      (create-distribution-package))))

(let ([cmd (cdr (command-line))]
      [src-path "src"]
      [obj-path "build"]
      [abslute-path (lambda (path)
                      (let ([head (string-ref path 0)]
                            [remain (substring path 1 (string-length path))])
                        (if (eqv? (directory-separator) head)
                            path
                            ;; ~/path
                            (if (eqv? #\~  head)
                                (let ([home-path (cd (string #\~))])
                                  (set! home-path (cd))
                                  (display home-path) (newline)
                                  (string-append home-path
                                                 remain))
                                (let ([cur-path (cd)])
                                  (if (eqv? #\. head)
                                      ;; ./path
                                      (string-append cur-path
                                                     remain)
                                      ;; path
                                      (string-append cur-path
                                                     (string (directory-separator))
                                                     path)))))))])
  ;; set src-path obj-path
  (unless (null? cmd)
          (set! src-path (car cmd))
          (set! cmd (cdr cmd))
          (unless (null? cmd)
                  (set! obj-path (car cmd))))
  ;; cover to abslute path
  (set! src-path (abslute-path src-path))
  (set! obj-path (abslute-path obj-path))

  (when (file-exists? src-path)
        ;; fix linked path
        (cd src-path)
        (set! src-path (cd))
        (make-dir obj-path)
        (cd obj-path)
        (set! obj-path (cd))
        ;; main
        (main src-path obj-path)))

;;; TODO: compile each source file
;;; TODO: make distribution
