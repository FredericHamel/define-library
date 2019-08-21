;;;============================================================================

;;; File: "define-library.scm"

;;; Copyright (c) 2014 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(##supply-module define-library)

(##namespace ("define-library#"))

(##include "~~lib/gambit#.scm")
(##include "~~lib/_gambit#.scm")
(##include "~~lib/_module#.scm")
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


(define (parts->path parts dir)
  (if (null? (cdr parts))
      (##path-expand (car parts) dir)
      (parts->path (cdr parts) (##path-expand (car parts) dir))))

;;;============================================================================

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
  cc-options
  ld-options
  pkg-config
  exports
  imports
  body
)

;; modref defined in _library#.scm
;;(define-type modref
;;  host        ;; ("github.com" "A") or ()
;;  tag         ;; ("tree" "1.0.0") or ()
;;  path)       ;; ("B" "C" "D")

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

(define (get-libdef modref modref-str reference-src)
  (define (err src)
    (##raise-expression-parsing-exception
     'cannot-find-library
     src
     (##desourcify src)))

  (let ((mod-info (or (##search-module modref)
                      (and (##install-module modref)
                           (##search-module modref)))))
    (if mod-info
      (let ((mod-dir            (##vector-ref mod-info 0))
            (mod-filename-noext (##vector-ref mod-info 1))
            (ext                (##vector-ref mod-info 2))
            (mod-path           (##vector-ref mod-info 3))
            (port               (##vector-ref mod-info 4))
            (root               (##vector-ref mod-info 5))
            (path               (##vector-ref mod-info 6)))

        ;(##display (##vector-copy mod-info) ##stdout-port)
        ;(##newline ##stdout-port)

        (read-libdef-sld mod-path
                         reference-src
                         modref-str
                         port
                         root
                         path))
      (err reference-src))))

(define (read-first port)
  (let* ((rt
          (##readtable-copy-shallow (##current-readtable)))
         (re
          (##make-readenv port rt ##wrap-datum ##unwrap-datum #f '() #f))
         (first
          (##read-datum-or-eof re)))
    (close-input-port port)
    first))


(define (read-libdef-sld name reference-src import-name port
                         module-root modref-path)
  (let ((src (read-first port)))
    (if (eof-object? src)
      (##raise-expression-parsing-exception
       'define-library-expected
       (##make-source src `#(,(string->symbol name))))
      (parse-define-library src import-name module-root modref-path))))

(define (read-libdef-scm name reference-src import-name port)
  (parse-define-library (read-first port) import-name #f #f))

;; Toggle logging
(define debug-mode? #f)
(define debug-mode?-set!
  (lambda (x)
    (set! debug-mode? x)))

(define library-kinds #f)
(set! library-kinds
      (list
       (cons ".sld"
             (vector read-libdef-sld))
       (cons ".scm"
             (vector read-libdef-scm))))

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

    ;; Test if part is invalid
    (define (invalid-part? part)
      (char=? #\/ (string-ref part (- (string-length part) 1))))

    (define (parse-parts lst)
      (let loop ((lst lst) (rev-parts '()))
        (cond ((null? lst)
               (reverse rev-parts))
              ((pair? lst)
               (let ((x (car lst)))
                 (cond ((symbol? x)
                        (let ((part (symbol->string x)))
                          (if (invalid-part? part)
                            (library-name-err)
                            (loop (cdr lst)
                                  (cons part rev-parts)))))
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
                (let ((head-str (symbol->string head)))
                  (if (invalid-part? head-str)
                    (library-name-err)
                    (let ((repo-parts (path->parts head-str)))
                      (if repo-parts
                          (append repo-parts (parse-parts (cdr spec)))
                          (parse-parts spec))))))))))

(define (parse-define-library src modref-str module-root modref-path)

  (define-type ctx
    id: ctx
    (src unprintable:)
    (name-src unprintable:)
    name
    namespace
    exports-tbl
    imports-tbl
    rev-pkg-config
    rev-cc-options
    rev-ld-options
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

                  ((pkg-config)
                   (if (not (and (pair? args-srcs)
                                 (string? (##source-strip (car args-srcs)))))
                     (library-decl-err)
                     (begin
                       (ctx-rev-pkg-config-set!
                         ctx
                         (parse-string-args
                           (cons
                             (##source-strip (car args-srcs))
                             (ctx-rev-pkg-config ctx))
                           (cdr args-srcs) library-decl-err))
                       (parse-body ctx rest-srcs))))

                  ((cc-options)
                   (if (not (and (pair? args-srcs)
                                 (string? (##source-strip (car args-srcs)))))
                     (library-decl-err)
                     (begin
                       (ctx-rev-cc-options-set!
                         ctx
                         (parse-string-args
                           (cons
                             (##source-strip (car args-srcs))
                             (ctx-rev-cc-options ctx))
                           (cdr args-srcs) library-decl-err))
                       (parse-body ctx rest-srcs))))

                  ((ld-options)
                   (if (not (and (pair? args-srcs)
                                 (string? (##source-strip (car args-srcs)))))
                     (library-decl-err)
                     (begin
                       (ctx-rev-ld-options-set!
                         ctx
                         (parse-string-args
                           (cons
                             (##source-strip (car args-srcs))
                             (ctx-rev-ld-options ctx))
                           (cdr args-srcs) library-decl-err))
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
               (idmap (parse-import-set import-set-src (ctx-name ctx))))
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

  (define (parse-string-args base args-srcs err)
    (if (pair? args-srcs)
      (let ((args (##source-strip (car args-srcs)))
            (rest-srcs (cdr args-srcs)))
        (if (string? args)
          (parse-string-args (cons args base) rest-srcs err)
          (err)))
      base))

  (define (has-suffix? str suffix)
    (let ((str-len (string-length str))
          (suffix-len (string-length suffix)))
      (and (<= suffix-len str-len)
           (string=?
             suffix
             (substring str (- str-len suffix-len) str-len)))))

  (define (join-rev path lst)
    (if (pair? lst)
      (join-rev
        (path-expand path (car lst))
        (cdr lst))
      path))

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
           (let* ((module-ref (or modref-str (table-ref (##compilation-scope) '##module-ref #f)))
                  (module-root (table-ref (##compilation-scope) '##module-root module-root))
                  (modref-path (table-ref (##compilation-scope) '##modref-path modref-path))

                  (_ (if debug-mode?
                       (begin
                         (display "module-ref: ")
                         (display module-ref)
                         (newline)
                         (display "module-root: ")
                         (display module-root)
                         (newline)
                         (display "modref-path: ")
                         (display modref-path)
                         (newline))))

                  ;; ("A" "B" "C")
                  (name-default
                    (parse-name name-src))

                  ;; "A/B/C"
                  (name-default-string (parts->path name-default ""))

                  (library-name (if module-ref
                                  (cond
                                    ((symbol? module-ref)
                                     (##symbol->string module-ref))
                                    (else module-ref))
                                  name-default-string))

                  (modref (##string->modref library-name))

                  ;; Test if modref-path == .../ + name-default-string
                  (valid? (if (null? (macro-modref-host modref))
                            (string=? library-name name-default-string)
                            (let ((mod-path (macro-modref-path modref)))
                              (has-suffix?
                                (join-rev (car mod-path) (cdr mod-path))
                                name-default-string))))

                  (ctx
                   (make-ctx src
                             name-src
                             library-name
                             (if valid? ; namespace
                               (##modref->string modref #t)
                               (##raise-expression-parsing-exception
                                'invalid-module-name
                                name-src))
                             (make-table test: eq?)
                             (make-table test: eq?)
                             '() ; rev-pkg-config
                             '() ; rev-cc-options
                             '() ; rev-ld-options
                             '()
                             '())))

             (if debug-mode?
               (let ()
                 (println "parse-define-library==>compilation-scope: " (object->string (table->list (##compilation-scope))))
                 (println "parse-define-library==>module-ref: " module-ref)
                 (println "parse-define-library==>modref-str: " modref-str)
                 (println "parse-define-library==>library-name: " library-name)
                 (println)))

             ;; parse-body modify ctx
             (parameterize ((##module-aliases (##module-aliases)))
               (##extend-aliases-from-rpath
                modref-path
                module-root)

               (parse-body ctx body-srcs))

             (let* ((body (reverse (ctx-rev-body ctx)))
                    (macros (parse-macros ctx body))
                    (ctxsrc (ctx-src ctx)))
               (make-libdef
                ctxsrc
                (ctx-name-src ctx)
                (ctx-name ctx)
                (ctx-namespace ctx)

                (reverse! (ctx-rev-cc-options ctx))
                (reverse! (ctx-rev-ld-options ctx))
                (reverse! (ctx-rev-pkg-config ctx))

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

(define (parse-import-set import-set-src #!optional (ctx-library? #f))
  (let ((import-set (##source-strip import-set-src)))

    (define (import-set-err)
      (##raise-expression-parsing-exception
       'ill-formed-import-set
       import-set-src))

    (define (ill-formed-library-name)
      (##raise-expression-parsing-exception
       'ill-formed-library-name
       import-set-src))

    (define (count-dot str)
      (let ((len (string-length str)))
        (let loop ((i 0))
          (if (< i len)
            (case (string-ref str i)
              ((#\.) (loop (+ i 1)))
              (else i))
            (or ctx-library?
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
            (let ((idmap (parse-import-set (car args-srcs) ctx-library?)))
              (case head

                ((rename)
                 (let ((idmap (parse-import-set (car args-srcs) ctx-library?)))
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

                 (updir-count (count-dot path))

                 (import-name-path
                   (if ctx-library?
                     (if (> updir-count 0)
                       (path-expand
                         (substring path updir-count (string-length path))
                         (macro-string-not-empty
                           (path-directory* ctx-library? (- updir-count 1))
                           ill-formed-library-name))
                       path)

                     ;; Forbid global (import (..foo)) out of library context
                     (if (> updir-count 0)
                       (ill-formed-library-name)
                       path)))

                 (modref-alias (let ((modref (##string->modref import-name-path)))
                                 ;(if modref
                                    (##apply-module-alias modref)

                                    #;(##raise-expression-parsing-exception
                                     'ill-formed-import-set
                                     import-set-src
                                     (##desourcify import-set-src))))

                 (modref-alias-str (##modref->string modref-alias))

                 (ld
                     (get-libdef modref-alias modref-alias-str import-set-src)))

            (make-idmap
              import-set-src
              import-set-src
              modref-alias-str
              (libdef-namespace ld)
              (idmap-macros (libdef-exports ld))
              (and (null? (libdef-body ld)) (null? (libdef-imports ld)))
              (idmap-map (libdef-exports ld))))))

      (import-set-err))))

(define (print-and-return src)
  (for-each
    (lambda (line)
      (println (object->string line)))
    (cdr (##desourcify src)))
  src)

(define (define-library-expand src)
  (let* ((ld (parse-define-library src #f #f #f))
         (ld-imports
           (map (lambda (x)
                  (let* ((idmap (vector-ref x 0))
                         (imports (vector-ref x 1)))

                    `(##begin

                      ,@(let ((name-symbol (string->symbol (idmap-name idmap))))
                         ;; Special library
                         (if debug-mode?
                           (let ()
                             (println "define-library-expand==>name: " (object->string (idmap-name idmap)))
                             (println "define-library-expand==>namespace: " (idmap-namespace idmap))
                             (println)))

                         (if (idmap-only-export? idmap)
                           `()
                           `((##demand-module ,name-symbol))))

                      ,@(let ((imports-list (table->list imports)))
                          (if (null? imports-list)
                            '()
                            `((##namespace
                               (,(idmap-namespace idmap)
                                 ,@(map (lambda (i)
                                          (if (eq? (car i) (cdr i))
                                            (car i)
                                            (list (cdr i) (car i))))
                                        imports-list))))))

                      ,@(apply
                          append
                          (map (lambda (m)
                                 (let ((id (car m)))
                                   (if (table-ref imports id #f) ;; macro is imported?
                                     `((##define-syntax
                                        (##lambda (src)
                                         (syn#apply-rules
                                           (##quote ,(cdr m))
                                           src))))
                                     '())))
                               (idmap-macros idmap))))))

                (libdef-imports ld))))

    (if debug-mode?
      (let ()
        (println "define-library-expand==>libdef-name: " (object->string (libdef-name ld)))
        (println "define-library-expand==>libdef-namespace: " (libdef-namespace ld))
        (println)))

    (##expand-source-template
     src
     (if (null? ld-imports)
       `(##begin) ;; empty library
       `(##begin
         (##supply-module ,(string->symbol (libdef-name ld)))
         ,@(let ((build-options-arguments
                  `(,@(if (null? (libdef-cc-options ld))
                        `()
                        `((cc-options ,(libdef-cc-options ld))))
                     ,@(if (null? (libdef-ld-options ld))
                         '()
                         `((ld-options ,(libdef-ld-options ld))))
                     ,@(if (null? (libdef-pkg-config ld))
                         '()
                         `((pkg-config ,(libdef-pkg-config ld)))))))
            (if (null? build-options-arguments)
              '()
              `((##quote (##build-options ,@build-options-arguments)))))
         (##namespace (,(libdef-namespace ld)))
         ,@ld-imports
         ,@(libdef-body ld)
         (##namespace ("")))))))

(define (import-expand src)
  ;; Local ctx
  (define rev-global-imports '())

  (define (directory-separator? pattern pos)
    (char=? (string-ref pattern pos) #\/))

  (if debug-mode?
    (begin
      (display "module-ref: ")
      (display (##table-ref (##compilation-scope) '##module-ref #f))
      (newline)
      (display "module-root: ")
      (display (##table-ref (##compilation-scope) '##module-root #f))
      (newline)
      (display "modref-path: ")
      (display (##table-ref (##compilation-scope) '##modref-path #f))
      (newline)))

  (let* ((locat (##source-locat src))
         (locat-filename (and (##vector? locat) (##vector-ref locat 0)))
         (rpath-root (if (or (##not locat-filename) ;; locat-filename is #f
                             (and (##pair? locat-filename)
                                  (##eq? (##car locat-filename) 'console)))
                       (##vector
                        '()
                        (##current-directory))
                       (##vector
                        (##table-ref (##compilation-scope) 'modref-path '())
                        (##table-ref (##compilation-scope) 'module-root (##path-directory locat-filename))))))
    ;; Clone alias environment.
    (parameterize ((##module-aliases (##module-aliases)))
      (##extend-aliases-from-rpath
       (##vector-ref rpath-root 0)
       (##vector-ref rpath-root 1))
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

              `(##begin

                ;; Imports dynamic (for functions)
                ,(let ((symbol-name (string->symbol (idmap-name idmap))))
                   ;; Special library
                   (if debug-mode?
                     (let ()
                       (println "import-expand==>import: " symbol-name)
                       (println "import-expand==>namespace: " (idmap-namespace idmap))
                       (println)))

                   (if (idmap-only-export? idmap)
                     `(##begin)
                     `(##demand-module ,symbol-name)))


                ,@(if (pair? (idmap-map idmap))
                   `((##namespace (,(idmap-namespace idmap)
                      ,@(map (lambda (i)
                               (if (eq? (car i) (cdr i))
                                 (car i)
                                 (list (car i) (cdr i))))
                             (idmap-map idmap)))))
                   '())

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
            rev-global-imports))))))

(define-runtime-syntax import
   import-expand)

(define-runtime-syntax define-library
  define-library-expand)

;;;============================================================================
