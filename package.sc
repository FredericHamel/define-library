#!/usr/bin/gsc-script -i package.sc
(##require-module _build)

(build#setup)
(build#link
  (build#make-project!
    (build#add-sources preload: #f "define-library.scm")))

