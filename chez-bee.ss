#!/usr/bin/scheme --script

(import (rnrs base))

;;; display license
(define display-license
  (lambda ()
    (display "MIT License") (newline)
    (display "Copyright (c) 2018 Z.Riemann") (newline)
    (display "https://github.com/ZRiemann/chez-bee") (newline)
    (newline)))
(display-license)

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
           (for-each handler files)
           (when recursive?
                 (set! dirs
                       (map (lambda (dir)
                              (string-append cur-path "/" dir))
                            dirs))
                 (for-each ftw dirs)))))]))

;;; check <fname> is Chez Scheme source file
;;;
;;; (chez-source? "src.ss") => (#t . "src.so")
;;; (chez-source? "not-src.xxx") => (#f . '())
(define-syntax chez-source?
  (syntax-rules ()
    [(_ fname)
     (call/cc
      (lambda (break)
        (for-each (lambda (extension)
                    (let ([ext (car extension)]
                          [len (string-length fname)]
                          [ext-len (string-length (car extension))])
                      (when (and (> len ext-len)
                                 (string=? ext (substring fname
                                                          (- len ext-len)
                                                          len)))
                                 (break (cons #t
                                              (string-append (substring fname 0 (- len ext-len))
                                                             (cdr extension)))))))
                  (library-extensions))
        (cons #f '())))]))

(define-syntax bee-mkdir
    (syntax-rules ()
      [(_ path)
       (unless (file-exists? path)
               (let mkparent ([parent (path-parent path)]
                              [mk-path path])
                 (unless (file-exists? parent)
                         (mkparent (path-parent parent) parent))
                 (mkdir mk-path)))]))

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
                                  (bee-mkdir (string-append obj-path
                                                           (string (directory-separator))
                                                           diff-path)))
                          ;; (generate-inspector-infomation #f)
                          ;; (strip-fasl-file)
                          ;; (make-boot-file "app.boot" '("scheme") "app.so")
                          (maybe-compile-file src-name obj-name)))]
           [build-depedents (lambda (src) src)]
           [compile-file (lambda (fname)
                           (let* ([res (chez-source? fname)]
                                  [obj (cdr res)])
                             (when (car res)
                                   (check-obj fname obj))))]
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
        (bee-mkdir obj-path)
        (cd obj-path)
        (set! obj-path (cd))
        ;; main
        (main src-path obj-path)))

;;; TODO: compile each source file
;;; TODO: make distribution
