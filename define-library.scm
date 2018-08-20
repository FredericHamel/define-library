;;;============================================================================

;;; File: "define-library.scm"

;;; Copyright (c) 2014 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(define-macro (dummy)
  (table-set! (##compilation-scope) 'module-name "_define-library")
  (table-set! (##compilation-scope) 'linker-name "_define-library")
  #f)
(dummy) ;; Set namespace to _define-library

(##namespace ("dl#"))
(##include "~~lib/gambit#.scm")
(##include "~~lib/_gambit#.scm")
(##include "~~lib/_library#.scm")
;;;============================================================================

;; Setup implementation of define-syntax and syntax-rules.
(##include "syntax.scm")
(##include "syntaxrulesxform.scm")

(define-runtime-syntax define-syntax
  (lambda (src)
    (let ((locat (##source-locat src)))
      (##make-source
       (##cons (##make-source '##define-syntax locat)
               (##cdr (##source-code src)))
       locat))))

(define-runtime-syntax syntax-rules
  syn#syntax-rules-form-transformer)

;;;============================================================================

(define (keep keep? lst)
  (cond ((null? lst)       '())
        ((keep? (car lst)) (cons (car lst) (keep keep? (cdr lst))))
        (else              (keep keep? (cdr lst)))))

(set! ##expression-parsing-exception-names
      (append
       '(
         (cannot-find-library            . "Cannot find library")
         (define-library-expected        . "define-library form expected")
         (ill-formed-library-name        . "Ill-formed library name")
         (ill-formed-library-decl        . "Ill-formed library declaration")
         (ill-formed-export-spec         . "Ill-formed export spec")
         (ill-formed-import-set          . "Ill-formed import set")
         (duplicate-identifier-export    . "Duplicate export of identifier")
         (duplicate-identifier-import    . "Duplicate import of identifier")
         (unexported-identifier          . "Library does not export identifier")
         )
       ##expression-parsing-exception-names))

(define (lib-name->namespace name)
  (apply string-append
         (map (lambda (x) (string-append x "#"))
              name)))

(define-type idmap
  id: idmap
  (src unprintable:)
  (name-src unprintable:)
  name
  namespace
  macros
  only-export?
  map
)

(define-type libdef
  (src unprintable:)
  (name-src unprintable:)
  name
  namespace
  exports
  imports
  body
)

;; libref defined in _library#.scm
;;(define-type libref
;;  host        ;; ("github.com" "A") or ()
;;  tag         ;; ("tree" "1.0.0") or ()
;;  path)       ;; ("B" "C" "D")

(define (has-prefix? str prefix)
  (and (string? str)
       (string? prefix)
       (let ((len-str (string-length str))
             (len-prefix (string-length prefix)))
         (and (>= len-str len-prefix)
              (string=? (substring str 0 len-prefix) prefix)
              (substring str len-prefix len-str)))))

;; Test if a valid hostname
(define (hostname? name)
  ; [A-Za-z0-9]
  (define (alphanum-ci? c)
    (or (and (char>=? c #\A) (char<=? #\Z))
        (and (char>=? c #\a) (char<=? #\z))
        (and (char>=? c #\0) (char<=? #\9))))

  (define (parse-segment-start name num-segment index name-len)
    (and (< index name-len)
      (let ((c (string-ref name index)))
        (and
          (alphanum-ci? c)
          (parse-segment name num-segment (+ index 1) name-len)))))

  (define (parse-segment name num-segment index name-len)
    (let loop ((i index)
               (last-alphanum? #t))
      (if (< i name-len)
        (let ((c (string-ref name i)))
          (cond
            ((char=? c #\-)
             (loop (+ i 1) #f))

            ((char=? c #\.)
             (and last-alphanum?
                  (parse-segment-start name (+ num-segment 1) (+ i 1) name-len)))

            ((alphanum-ci? c)
             (loop (+ i 1) #t))

            (else
              ;; Character not allowed
              #f)))

        (> num-segment 0))))

  (let ((name-len (string-length name)))
    (parse-segment-start name 0 0 name-len)))

(define (path->parts path)
  (let ((path-len (string-length path)))
    (define (split-path start rev-result)
      (let loop ((end start))
        (if (< end path-len)
          (case (string-ref path end)
            ((#\/)
             (split-path (+ end 1)
                         (if (> end start)
                           (cons (substring path start end) rev-result)
                           rev-result)))
            (else
              (loop (+ end 1))))
            (reverse
              (if (> end start)
                (cons (substring path start end) rev-result)
                rev-result)))))
    (split-path 0 '())))

(define (get-libdef name reference-src)
  (let* ((name-str (parts->path name ""))
         (libref (##string->libref name-str)))
    (let ((lib (##search-library libref)))
      (if lib
        (begin
          (parameterize ((##compilation-scope (make-table test: eq?)))
            (table-set! (##compilation-scope) 'library-name
                        (and (pair? (macro-libref-host libref)) name-str))
            (read-libdef-sld name reference-src (cdr lib))))

        (##raise-expression-parsing-exception
         'cannot-find-library
         reference-src
         (##desourcify reference-src))))))

(define (read-first port)
  (let* ((rt
          (##readtable-copy-shallow (##current-readtable)))
         (re
          (##make-readenv port rt ##wrap-datum ##unwrap-datum #f #f))
         (first
          (##read-datum-or-eof re)))
    (close-input-port port)
    first))


(define (read-libdef-sld name reference-src port)
  (parse-define-library (read-first port)))

(define (read-libdef-scm name reference-src port)
  (parse-define-library (read-first port)))

;; Toggle logging
(define debug-mode #f)

(define library-user-location
  ;; userlib
  (path-expand (##os-path-gambitdir-map-lookup "userlib")))

(define library-locations
  (list #f        ;; #f means relative to source file
;        ""        ;; "" means current directory
        library-user-location
        "~~lib")) ;; lib directory in Gambit installation directory

(define library-kinds #f)
(set! library-kinds
      (list

       (cons ".sld"
             (vector read-libdef-sld))
       (cons ".scm"
             (vector read-libdef-scm))))

(define (println-log . msg)
  (if debug-mode
    (apply println msg)))

(define (parts->path parts dir)
  (if (null? (cdr parts))
      (##path-expand (car parts) dir)
      (parts->path (cdr parts) (##path-expand (car parts) dir))))

(define (read-file-as-a-begin-expr lib-decl-src filename-src)
  (let ((filename (##source-strip filename-src)))
    (if (##string? filename)

        (let* ((locat
                (##source-locat lib-decl-src))
               (relative-to-path
                (and locat
                     (##container->path (##locat-container locat)))))
          (let* ((path
                  (##path-reference filename relative-to-path))
                 (x
                  (##read-all-as-a-begin-expr-from-path
                   path
                   (##current-readtable)
                   ##wrap-datum
                   ##unwrap-datum)))
            (if (##fixnum? x)
                (##raise-expression-parsing-exception
                 'cannot-open-file
                 lib-decl-src
                 path)
                (##vector-ref x 1))))

        (##raise-expression-parsing-exception
         'filename-expected
         filename-src))))

(define (parse-name name-src)

    (define (library-name-err)
      (##raise-expression-parsing-exception
       'ill-formed-library-name
       name-src))

    (define (parse-parts lst)
      (let loop ((lst lst) (rev-parts '()))
        (cond ((null? lst)
               (reverse rev-parts))
              ((pair? lst)
               (let ((x (car lst)))
                 (cond ((symbol? x)
                        (loop (cdr lst)
                              (cons (symbol->string x) rev-parts)))
                       ((and (integer? x)
                             (exact? x)
                             (>= x 0))
                        (loop (cdr lst)
                              (cons (number->string x) rev-parts)))
                       (else
                        (library-name-err)))))
              (else
               (library-name-err)))))
    (let ((spec (##desourcify name-src)))
      (if (not (pair? spec))
          (library-name-err)
          (let ((head (car spec)))
            (if (memq head '(rename prefix only except))
                (library-name-err) ;; conflict with import declaration syntax
                (let ((repo-parts (path->parts (symbol->string head))))
                  (if repo-parts
                      (append repo-parts (parse-parts (cdr spec)))
                      (parse-parts spec))))))))

(define (parse-define-library src)

  (define-type ctx
    id: ctx
    (src unprintable:)
    (name-src unprintable:)
    name
    namespace
    exports-tbl
    imports-tbl
    rev-imports
    rev-body
  )

  (define (parse-body ctx body-srcs)
    (if (pair? body-srcs)
        (let* ((lib-decl-src (car body-srcs))
               (lib-decl (##source-strip lib-decl-src))
               (rest-srcs (cdr body-srcs)))

          (define (library-decl-err)
            (##raise-expression-parsing-exception
             'ill-formed-library-decl
             lib-decl-src))

          (if (pair? lib-decl)

              (let* ((head-src (car lib-decl))
                     (head (##source-strip head-src))
                     (args-srcs (cdr lib-decl)))
                (case head

                  ((export)
                   (parse-export-decl ctx args-srcs)
                   (parse-body ctx rest-srcs))

                  ((import)
                   (parse-import-decl ctx args-srcs)
                   (parse-body ctx rest-srcs))

                  ((begin)
                   (parse-begin-decl ctx args-srcs)
                   (parse-body ctx rest-srcs))

                  ((include include-ci include-library-declarations)
                   (parse-body
                    ctx
                    (append (parse-include
                             ctx
                             args-srcs
                             lib-decl-src
                             head)
                            rest-srcs)))

                  ((cond-expand)
                   ;;TODO: actually implement
                   (parse-body ctx rest-srcs))

                  ((namespace) ;; extension to R7RS
                   (if (not (and (pair? args-srcs)
                                 (null? (cdr args-srcs))
                                 (string? (##source-strip (car args-srcs)))))
                       (library-decl-err)
                       (begin
                         (ctx-namespace-set!
                          ctx
                          (##source-strip (car args-srcs)))
                         (parse-body ctx rest-srcs))))

                  (else
                   (library-decl-err))))

              (library-decl-err)))))

  (define (add-identifier-export! ctx internal-id-src external-id-src)
    (let* ((internal-id (##source-strip internal-id-src))
           (external-id (##source-strip external-id-src)))
      (if (table-ref (ctx-exports-tbl ctx) external-id #f)
          (##raise-expression-parsing-exception
           'duplicate-identifier-export
           external-id-src
           external-id)
          (table-set! (ctx-exports-tbl ctx) external-id internal-id))))

  (define (add-imports! ctx import-set-src idmap)
    (let* ((name
            (idmap-name idmap))
           (x
            (assoc name (ctx-rev-imports ctx)))
           (tbl
            (if x
                (vector-ref (cdr x) 1)
                (let ((tbl (make-table test: eq?)))
                  (ctx-rev-imports-set!
                   ctx
                   (cons (cons name (vector idmap tbl))
                         (ctx-rev-imports ctx)))
                  tbl))))
      (for-each (lambda (x)
                  (let* ((external-id (if (symbol? x) x (car x)))
                         (internal-id (if (symbol? x) x (cdr x)))
                         (already-imported-from
                          (table-ref (ctx-imports-tbl ctx) external-id #f)))
                    (if (and already-imported-from
                             ;; ignore redundant imports from a given library
                             (not (equal? (idmap-namespace idmap)
                                          already-imported-from)))
                        (##raise-expression-parsing-exception
                         'duplicate-identifier-import
                         import-set-src
                         external-id)
                        (begin
                          (table-set! (ctx-imports-tbl ctx)
                                      external-id
                                      (idmap-namespace idmap))
                          (table-set! tbl internal-id external-id)))))
                (idmap-map idmap))))

  (define (parse-export-decl ctx export-specs-srcs)
    (if (pair? export-specs-srcs)
        (let* ((export-spec-src (car export-specs-srcs))
               (export-spec (##source-strip export-spec-src))
               (rest-srcs (cdr export-specs-srcs)))

          (define (export-spec-err)
            (##raise-expression-parsing-exception
             'ill-formed-export-spec
             export-spec-src))

          (cond ((symbol? export-spec)
                 (add-identifier-export!
                  ctx
                  export-spec-src
                  export-spec-src)
                 (parse-export-decl ctx rest-srcs))

                ((pair? export-spec)
                 (let* ((head-src (car export-spec))
                        (head (##source-strip head-src))
                        (args-srcs (cdr export-spec)))
                   (case head

                     ((rename)
                      (if (not (and (pair? args-srcs)
                                    (pair? (cdr args-srcs))
                                    (null? (cddr args-srcs))))
                          (export-spec-err)
                          (let* ((internal-id-src (car args-srcs))
                                 (internal-id (##source-strip internal-id-src))
                                 (external-id-src (cadr args-srcs))
                                 (external-id (##source-strip external-id-src)))
                            (if (not (and (symbol? internal-id)
                                          (symbol? external-id)))
                                (export-spec-err)
                                (begin
                                  (add-identifier-export!
                                   ctx
                                   internal-id-src
                                   external-id-src)
                                  (parse-export-decl ctx rest-srcs))))))

                     (else
                      (export-spec-err)))))

                (else
                 (export-spec-err))))))

  (define (parse-import-decl ctx import-sets-srcs)
    (if (pair? import-sets-srcs)
        (let* ((import-set-src (car import-sets-srcs))
               (rest-srcs (cdr import-sets-srcs))
               ;;; Why ctx in parse-import-set? Cause not used...
               (idmap (parse-import-set import-set-src (parts->path (ctx-name ctx) ""))))
          (add-imports! ctx import-set-src idmap)
          (parse-import-decl ctx rest-srcs))))

  (define (parse-begin-decl ctx body-srcs)
    (ctx-rev-body-set! ctx (append (reverse body-srcs) (ctx-rev-body ctx))))

  (define (parse-include ctx filenames-srcs lib-decl-src kind)
    (if (pair? filenames-srcs)
        (let* ((filename-src (car filenames-srcs))
               (rest-srcs (cdr filenames-srcs))
               (x (read-file-as-a-begin-expr lib-decl-src filename-src)))
          (append (if (eq? kind 'include-library-declarations)
                      (cdr (##source-strip x))
                      (list (cons 'begin (cdr (##source-strip x)))))
                  (parse-include ctx rest-srcs lib-decl-src kind)))
        '()))

  (define (parse-macros ctx body)
    (let loop ((expr-srcs body) (rev-macros '()))

      (define (done)
        (reverse rev-macros))

      (if (not (pair? expr-srcs))
          (done)
          (let* ((expr-src (car expr-srcs))
                 (expr (##source-strip expr-src)))
            (if (not (and (pair? expr)
                          (eq? (##source-strip (car expr)) 'define-syntax)
                          (pair? (cdr expr))
                          (symbol? (##source-strip (cadr expr)))
                          (pair? (cddr expr))
                          (let ((x (##source-strip (caddr expr))))
                            (and (pair? x)
                                 (eq? (##source-strip (car x)) 'syntax-rules)))
                          (null? (cdddr expr))))
                (done)
                (let ((id (##source-strip (cadr expr)))
                      (crules (syn#syntax-rules->crules (caddr expr))))

                  (define (generate-local-macro-def id crules expr-src)
                    (let ((locat (##source-locat expr-src)))
                      (##make-source
                       `(##define-syntax ,id
                          (##lambda (##src)
                            (syn#apply-rules ',crules ##src)))
                       locat)))

                  ;; replace original define-syntax by local macro def
                  ;; to avoid having to load syntax-rules implementation
                  (set-car! expr-srcs
                            (generate-local-macro-def id crules expr-src))

                  (loop (cdr expr-srcs)
                        (cons (cons id crules)
                              rev-macros))))))))

  (define (lib-source-filename->namespace filename)
    (let ((name-space (string-append (path-strip-extension filename) "#")))
      (let ((namespace-len (string-length name-space)))
        (let loop ((i 0))
          (if (< i namespace-len)
            (case (string-ref name-space i)
              ((#\/)
               (string-set! name-space i #\#)
               (loop (+ i 1)))
              (else
                (loop (+ i 1))))
            name-space)))))

  (define (has-suffix? str suffix)
    (let ((str-len (string-length str))
          (suffix-len (string-length suffix)))
      (and (<= suffix-len str-len)
           (string=?
             suffix
             (substring str (- str-len suffix-len) str-len)))))

  (define (join-rev parts dir)
    (if (pair? parts)
      (join-rev
        (cdr parts)
        (path-expand
          (car parts) ; B
          dir)) ; C -> C/B
      dir))

  (let ((form (##source-strip src)))
    (if (not (and (pair? form)
                  (eq? 'define-library (##source-strip (car form)))))

        (##raise-expression-parsing-exception
         'define-library-expected
         src)

        (##deconstruct-call
         src
         -2
         (lambda (name-src . body-srcs)
           (let* ((name
                   (parse-name name-src))

                  (name-str (parts->path name ""))

                  ;; Name it is imported or compiled with.
                  (library-name (or (table-ref (##compilation-scope) 'library-name #f)
                                    (table-ref (##compilation-scope) 'module-name #f)
                                    name-str))

                  (libref (##string->libref library-name))

                  (valid? (or
                            (null? (macro-libref-host libref))
                            (has-suffix?
                              (join-rev (macro-libref-path libref) "")
                              name-str)))

                  (ctx
                   (make-ctx src
                             name-src
                             name
                             (if valid?
                               (##libref->string libref #t)
                               (error "Invalid namespace"))
                             (make-table test: eq?)
                             (make-table test: eq?)
                             '()
                             '())))
             (parse-body ctx body-srcs)

             (let* ((body (reverse (ctx-rev-body ctx)))
                    (macros (parse-macros ctx body))
                    (ctxsrc (ctx-src ctx)))
               (make-libdef
                ctxsrc
                (ctx-name-src ctx)
                (ctx-name ctx)
                (ctx-namespace ctx)

                (make-idmap
                 (ctx-src ctx)
                 (ctx-name-src ctx)
                 (ctx-name ctx)
                 (ctx-namespace ctx)
                 macros
                 (and (null? body) (null? (ctx-rev-imports ctx))) ;; Replace with good value
                 (table->list (ctx-exports-tbl ctx)))

                (map cdr (reverse (ctx-rev-imports ctx)))

                body))))))))

(define (parse-import-set import-set-src #!optional (ctx-library #f))
  (let ((import-set (##source-strip import-set-src)))

    (define (import-set-err)
      (##raise-expression-parsing-exception
       'ill-formed-import-set
       import-set-src))

    (define (ill-formed-library-name)
      (##raise-expression-parsing-exception
       'ill-formed-library-name
       import-set-src))

    (define (count-point str)
      (let ((len (string-length str)))
        (let loop ((i 0))
          (if (< i len)
            (case (string-ref str i)
              ((#\.) (loop (+ i 1)))
              (else i))
            (or ctx-library
                ;; only in global import.
                (ill-formed-library-name))))))


    (define (path-directory* dir num)
      (if (> num 0)
        (path-directory*
          (path-strip-trailing-directory-separator (path-directory dir))
          (- num 1))
        dir))

    (define-macro (macro-string-not-empty str err)
      (let ((g0 (gensym)))
        `(let ((,g0 ,str))
           (if (= (string-length ,g0) 0)
             (,err)
             ,g0))))


    (if (pair? import-set)

      (let* ((head-src (car import-set))
             (head (##source-strip head-src))
             (args-srcs (cdr import-set)))
        (if (memq head '(rename prefix only except))

          (if (not (pair? args-srcs))
            (import-set-err)
            (let ((idmap (parse-import-set (car args-srcs) ctx-library)))
              (case head

                ((rename)
                 (let ((idmap (parse-import-set (car args-srcs) ctx-library)))
                   (let loop ((lst (cdr args-srcs)) (renames '()))
                     (cond ((null? lst)
                            (make-idmap
                              import-set-src
                              (idmap-name-src idmap)
                              (idmap-name idmap)
                              (idmap-namespace idmap)
                              (idmap-macros idmap)
                              (idmap-only-export? idmap)
                              (append (map (lambda (r)
                                             (cons (cdr r) (cdar r)))
                                           renames)
                                      (keep (lambda (x)
                                              (not (assq x renames)))
                                            (idmap-map idmap)))))
                           ((pair? lst)
                            (let* ((ren-src (car lst))
                                   (ren (##source-strip ren-src)))
                              (if (not (and (pair? ren)
                                            (pair? (cdr ren))
                                            (null? (cddr ren))))
                                (import-set-err)
                                (let* ((id1-src (car ren))
                                       (id1 (##source-strip id1-src))
                                       (id2-src (cadr ren))
                                       (id2 (##source-strip id2-src)))
                                  (if (not (and (symbol? id1)
                                                (symbol? id2)))
                                    (import-set-err)
                                    (let ((x
                                            (assq id1 (idmap-map idmap))))
                                      (if (not x)
                                        (##raise-expression-parsing-exception
                                         'unexported-identifier
                                         id1-src
                                         id1)
                                        (loop (cdr lst)
                                              (cons (cons x id2)
                                                    renames)))))))))
                           (else
                             (import-set-err))))))

                ((prefix)
                 (if (not (and (pair? (cdr args-srcs))
                               (null? (cddr args-srcs))))
                   (import-set-err)
                   (let* ((prefix-src
                            (cadr args-srcs))
                          (prefix
                            (##source-strip prefix-src)))
                     (if (not (symbol? prefix))
                       (import-set-err)
                       (make-idmap
                         import-set-src
                         (idmap-name-src idmap)
                         (idmap-name idmap)
                         (idmap-namespace idmap)
                         (idmap-macros idmap)
                         (idmap-only-export? idmap)
                         (let ((prefix-str (symbol->string prefix)))
                           (map (lambda (x)
                                  (cons (string->symbol
                                          (string-append
                                            prefix-str
                                            (symbol->string (car x))))
                                        (cdr x)))
                                (idmap-map idmap))))))))

                (else
                  (let* ((ids
                          (map (lambda (id-src)
                                 (let ((id (##source-strip id-src)))
                                   (if (not (symbol? id))
                                     (##raise-expression-parsing-exception
                                      'id-expected
                                      id-src)
                                     (if (not (assq id (idmap-map idmap)))
                                       (##raise-expression-parsing-exception
                                        'unexported-identifier
                                        id-src
                                        id)
                                       id))))
                               (cdr args-srcs)))
                        (only-keep (if (eq? head 'only)
                              (lambda (x) (memq (car x) ids))
                              (lambda (x) (not (memq (car x) ids))))))
                    (make-idmap
                      import-set-src
                      (idmap-name-src idmap)
                      (idmap-name idmap)
                      (idmap-namespace idmap)
                      (keep only-keep (idmap-macros idmap))
                      (idmap-only-export? idmap)
                      (keep only-keep (idmap-map idmap))))))))

          (let* ((name
                   (parse-name import-set-src))
                 (path (parts->path name ""))
                 (updir-count (count-point path))
                 (import-name (if ctx-library
                                (if (> updir-count 0)
                                  (path->parts
                                    (path-expand
                                      (substring path updir-count (string-length path))
                                      (macro-string-not-empty
                                        (path-directory* ctx-library (- updir-count 1))
                                        ill-formed-library-name)))
                                  name)
                                ;; Forbid global (import (..foo))
                                (if (> updir-count 0)
                                  (ill-formed-library-name)
                                  name)))

                 (name-with-symbols
                   (map string->symbol import-name))
                 (ld
                   (get-libdef import-name import-set-src)))
            (make-idmap
              import-set-src
              import-set-src
              import-name
              (libdef-namespace ld)
              (idmap-macros (libdef-exports ld))
              (and (null? (libdef-body ld)) (null? (libdef-imports ld)))
              (idmap-map (libdef-exports ld))))))

      (import-set-err))))

(define (import->symbolic-string name)
  (parts->path name ""))

(define (print-and-return src)
  (for-each
    (lambda (line)
      (println (object->string line)))
    (cdr (##desourcify src)))
  src)

(define (resolve-relative-path filename)
  (let loop ((paths library-locations))
    (and
      (pair? paths)
      (let* ((prefix (or (car paths) (current-directory)))
             (result
              (has-prefix?
                filename
                (if (car paths)
                  (path-expand (car paths))
                  (current-directory)))))
        (or result
          (loop (cdr paths)))))))

(define (define-library-expand src)
  (let* ((ld (parse-define-library src))
         (ld-imports
           (map (lambda (x)
                  (let* ((idmap (vector-ref x 0))
                         (imports (vector-ref x 1)))
                         ;; TODO: Reimplement relative import.
                         ;(from-file (resolve-relative-path (##vector-ref (##source-locat (idmap-src idmap)) 0))))

                    `(##begin
                      ;; TODO: Detect host library import
                      ,(let ((symbol-name (string->symbol (import->symbolic-string (idmap-name idmap)))))
                         ;; Special library
                         (if (idmap-only-export? idmap)
                           `(##begin)
                           `(##require-module ,symbol-name)))

                      ,@(if (null? imports)
                          '()
                          `((##namespace
                             (,(idmap-namespace idmap)
                               ,@(map (lambda (i)
                                        (if (eq? (car i) (cdr i))
                                          (car i)
                                          (list (cdr i) (car i))))
                                      (table->list imports))))))

                      ,@(apply
                          append
                          (map (lambda (m)
                                 (let ((id (car m)))
                                   (if (table-ref imports id #f) ;; macro is imported?
                                     `((##define-syntax
                                        ,(string->symbol
                                           (string-append
                                             (idmap-namespace idmap)
                                             (symbol->string id)))
                                        (##lambda (src)
                                         (syn#apply-rules
                                           (##quote ,(cdr m))
                                           src))))
                                     '())))
                               (idmap-macros idmap))))))

                (libdef-imports ld))))
    (##expand-source-template
     src
     (if (null? ld-imports)
       (if (null? (libdef-body ld))
         `(##begin) ;; empty library
         (error "Cannot have body"))
       `(##begin
         (##namespace (,(libdef-namespace ld)))
         ,@ld-imports
         ,@(libdef-body ld)
         (##namespace ("")))))))

(define (import-expand src)
  ;; Local ctx
  (define rev-global-imports '())

  (##deconstruct-call
   src
   -2
   (lambda args-srcs
     (map
       (lambda (args-src)
         (let ((idmap (parse-import-set args-src)))
           (set! rev-global-imports (cons idmap rev-global-imports))))
       args-srcs)))

  (##expand-source-template
   src
   `(##begin
     ,@(map (lambda (idmap)
              ;; Relative import within module (not completed yet)

              ;(let ((from-file (resolve-relative-path (##vector-ref (##source-locat (idmap-src idmap)) 0))))

              `(##begin

                ;; Imports dynamic (for functions)
                ,(let ((symbol-name (string->symbol (import->symbolic-string (idmap-name idmap)))))
                   ;; Special library
                   (if (idmap-only-export? idmap)
                     `(##begin)
                     `(##require-module ,symbol-name)))

                (##namespace
                 (,(idmap-namespace idmap)
                   ,@(map (lambda (i)
                            (if (eq? (car i) (cdr i))
                              (car i)
                              (list (car i) (cdr i))))
                          (idmap-map idmap))))

                ;; Macro Handler !!!
                ,@(apply
                    append
                    (map (lambda (m)
                           (let ((id (car m)))
                             (if #t ;(table-ref imports id #f) ;; macro is imported?
                               `((##define-syntax
                                  ,(string->symbol
                                     (string-append
                                       (idmap-namespace idmap)
                                       (symbol->string id)))
                                  (##lambda (src)
                                   (syn#apply-rules
                                     (##quote ,(cdr m))
                                     src))))
                               '())))
                         (idmap-macros idmap)))))
            rev-global-imports))))

(define-runtime-syntax import
   import-expand)

(define-runtime-syntax define-library
  define-library-expand)

;; register-loader

;; Add .sld
(##scheme-file-extensions-set!
 (cons '(".sld" . #f) ##scheme-file-extensions))

(##load-required-module-set!
 (lambda (module-ref)
  (define (library-not-found-exception libname)
    (##raise-module-not-found-exception
     ;; not sure if I should use this reference...
     ##load-required-module
     libname))
  (cond
    ((##symbol? module-ref)
     (let ((module (##lookup-registered-module module-ref)))
       (if module
         (##load-required-module-structs (##list module) #t)
         (##load-library
          (##string->libref (##symbol->string module-ref))
          #t))))
    (else
      (library-not-found-exception module-ref)))))

;;;============================================================================
