
(namespace ("dl#"
            debug-mode
            println-log
            has-prefix?))

(define ##loaded-libraries '())
(define ##sys-path (list #f "~~lib"))

;; Add .sld
(##scheme-file-extensions-set!
 (cons '(".sld" . #f) ##scheme-file-extensions))

(println (object->string ##scheme-file-extensions))
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


     ;; has-prefix? as define in define-library
     #;(define (has-prefix? str prefix)
       (let ((str-len (string-length str))
             (prefix-len (string-length prefix)))
         (and (> str-len prefix-len)
              (string=? prefix (substring str 0 prefix-len))
              (substring str prefix-len str-len))))

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
            ;; TODO: Handle https: local repo
            (let* ()#;((repo-parts
                     (call-with-input-string
                       has-repo?
                       (lambda (r)
                         (read-all
                           r (lambda (r) (read-line r #\/))))))
                   (module (package#url-parts->module-type #f repo-parts "tree")))
              ;;(println-log module-ref)
              (error "[register-loder] remote loader not implemented yet")
              (println-log
                (##path-expand
                 (##path-expand
                  (package#module-name module)
                  (package#module-version module))
                 (##path-expand (package#module-path module) package#location)))
              (let ((module-path
                      (##path-expand
                       (##path-expand
                        (package#module-name module)
                        (package#module-version module))
                       (##path-expand (package#module-path module) package#location)))
                    (module-symbol (##string->symbol module-path)))
                (let ((x (##load module-path
                          (lambda (script-line sc/ript-path) #f)
                          #t
                          #f
                          #t)))
                  (if (##fixnum? x)
                    (err)
                    (##begin
                     (or (memq module-symbol ##loaded-libraries)
                         (##set! ##loaded-libraries
                          (cons module-symbol ##loaded-libraries)))
                     x)))))
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
