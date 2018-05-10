#!/usr/bin/env gsc-script -i _build-dl.scm
(##require-module _build)

(build#setup)
(build#compile preload: #f "define-library.scm" )

(build#link-dynamic)

