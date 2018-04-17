
(define ##loaded-libraries '())
(define ##sys-path (list #f "~~lib"))

(namespace ("dl#"
            debug-mode
            path->parts
            println-log
            has-prefix?))

(namespace ("library#"
            path->host-library
            repo-parts->host-library

            ;; library exception
            library-not-found-exception

            ;; host-library-type
            make-library
            library-host
            library-username
            library-reponame
            library-treesep
            library-tag
            library-subdir))



;; Add .sld
(##scheme-file-extensions-set!
 (cons '(".sld" . #f) ##scheme-file-extensions))

(println (object->string ##scheme-file-extensions))

(define-type host-library-type
  constructor: make-library
  (host library-host)
  (username library-username)
  (repo-name library-reponame)
  (tree-sep library-treesep)
  ;; Version
  (tag library-tag)
  (subdir library-subdir))

(define (repo-parts->host-library parts)
  (define (parse-username host rest)
    (and (pair? rest)
         (parse-reponame host (car rest) (cdr rest))))

  (define (parse-reponame host username rest)
    (and (pair? rest)
         (parse-tree host username (car rest) (cdr rest))))

  (define (parse-tree host username repo-name rest)
    (and (pair? rest)
         (parse-tag host username repo-name (car rest) (cdr rest))))

  (define (parse-tag host username repo-name tree-sep rest)
    (and (pair? rest)
         (parse-subdir host username repo-name tree-sep (car rest) (cdr rest))))

  (define (parse-subdir host username repo-name tree-sep tag subdir)
    (make-library host username repo-name tree-sep tag subdir))

  ;; parse-host
  (and (pair? parts)
       (parse-username (car parts) (cdr parts))))

(define (path->host-library path)
  (repo-parts->host-library
    (path->parts path)))

(define (library-not-found-exception libname)
  (##raise-module-not-found-exception
   ;; not sure if I should use this reference...
   ##default-load-required-module
   libname))

(##load-required-module-set!
 (let ((old-load-required-module ##load-required-module))
   (lambda (module-ref)
     (define (err)
       (##raise-module-not-found-exception
        ##default-load-required-module
        module-ref))

     (define (module-resolve-in-path-indirect module-fullname path)
       (let ((module-path (path-expand module-fullname path)))
         (println-log "Try: " module-path)
         (and (file-exists? (string-append module-path ".sld")) module-path)))

     (define (module-resolve-in-path module-fullname module-canonical-name path)
       (let ((module-path (path-expand module-fullname path)))
         (println-log "Try: " module-path)
         (if (file-exists? (string-append module-path ".sld"))
           module-path
           (module-resolve-in-path-indirect
             (path-expand module-canonical-name module-fullname) path))))

     (define (module-resolve module-fullname)
       (let ((module-canonical-name
               (path-strip-extension
                 (path-strip-directory module-fullname))))
         (let loop ((paths ##sys-path))
           (if (pair? paths)
             ;; Look at this: Source of future bug
             (let ((path (or (car paths) (current-directory))))
               (if path
                 (let ((module-path (module-resolve-in-path
                                      module-fullname module-canonical-name (path-expand path))))
                   (if module-path
                     module-path
                     (loop (cdr paths))))))
             (err)))))

     (define (is-standard-lib? name)
       (let ((name-length (##string-length name)))
         (and (> name-length 5)
              (##string=? (##substring name 0 5) "~~lib")
              name)))

     (define (load-library-by-name library-name)
       (let ((x (##load library-name
                 (lambda (script-line script-path) #f)
                 #t #f #t)))
         (if (##fixnum? x)
           (err)
           (let ((library-name-symbol
                   (##string->symbol library-name)))
             (or (memq library-name-symbol ##loaded-libraries)
                 (##set! ##loaded-libraries
                  (cons library-name-symbol ##loaded-libraries)))
             x))))

     (cond
       ((vector? module-ref)
        (if (= (vector-length module-ref) 2)
          (let* ((module-name (vector-ref module-ref 0))
                 (module-canonical-name (path-strip-directory
                                          (path-strip-extension module-name)))
                 (from-file (vector-ref module-ref 1))
                 (from-path (and
                              (string=? (path-extension from-file) ".sld")
                              (module-resolve (path-strip-extension from-file)))))
            (or (and
                  from-path
                  (module-resolve-in-path module-name module-canonical-name from-path))
                (let ((module-path (module-resolve module-name)))
                  (if (boolean? module-path)
                    (println-log "There is a bug: " module-name)
                    (load-library-by-name module-path)))))))

       ((string? module-ref)
        (let ((has-repo? (or (has-prefix? module-ref "http:")
                             (has-prefix? module-ref "https:"))))
          (if has-repo?
            ;; host library
            (let ((host-lib (path->host-library has-repo?)))
              (println (object->string host-lib))
              (let ((lib-path (path-expand
                                (let ((subdir (library-subdir host-lib)))
                                  (if (null? subdir)
                                    (library-reponame host-lib)
                                    (parts->path subdir "")))
                                (path-expand
                                  (library-tag host-lib)
                                  (path-expand
                                    (library-reponame host-lib)
                                    (path-expand (library-username host-lib)
                                                 (library-host host-lib)))))))
                (let ((libname (module-resolve-in-path-indirect lib-path dl#library-user-location)))
                  (if (boolean? libname)
                    (println-log "[##load] bug")
                    (load-library-by-name libname)))))

            ;; local library
            (let ((module (##string->symbol module-ref)))
              (let* ((module-path (module-resolve module-ref))
                     (module-symbol (##string->symbol module-path)))
                (or ;; load module ignoring warning.
                  (and (memq module-symbol ##loaded-libraries)
                       module-path)
                  (let ((x (##load module-path
                            (lambda (script-line script-path) #f)
                            #t
                            #f
                            #t)))
                    (if (##fixnum? x)
                      (err)
                      (##begin
                       (or (memq module-symbol ##loaded-libraries)
                           (begin
                             (println-log "[##load]: " x)
                             (##set! ##loaded-libraries
                              (cons module-symbol ##loaded-libraries))))
                       x)))))))))
       (else
         (old-load-required-module module-ref))))))
