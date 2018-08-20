
(build#setup)
(build#link
  (build#make-project!
    (build#add-sources preload: #t "define-library.scm")))

