(library (bee file-system (1 0 0))
  (export bee-mkdir)
  (import (chezscheme) (only (chezscheme) mkdir))

  ;;; like shell mkdir -p ...
  ;;; no error if existing, make parent directories as needed
  (define-syntax bee-mkdir
    (syntax-rules ()
      [(_ path)
       (unless (file-exists? path)
               (let mkparent ([parent (path-parent path)]
                              [mk-path path])
                 (unless (file-exists? parent)
                         (mkparent (path-parent parent) parent))
                 (mkdir mk-path)))]))
  ;; more interfaces
  ;; ...
  )
