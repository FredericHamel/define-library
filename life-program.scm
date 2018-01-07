;; Main program.
(import (scheme base)
        (only (example life) life)
        (rename (prefix (example grid) grid-)
                (grid-make make-grid)))

;; grid-set! didn't work due to macro as variable

;; Initialize a grid with a glider
(define grid (make-grid 24 24))
(grid-put! grid 1 1 #t)
(grid-put! grid 2 2 #t)
(grid-put! grid 3 0 #t)
(grid-put! grid 3 1 #t)
(grid-put! grid 3 2 #t)

;; Run for 80 iterations.
(life grid 80)
