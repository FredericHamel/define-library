;; Main program.
(import (scheme base)
        (only (example life) life)
        (example grid)
        #;(rename (prefix (example grid) grid-)
                (grid-make make-grid)))

;; grid-set! didn't work due to macro as variable

;; Initialize a grid with a glider
(define grid (make 24 24))
(put! grid 1 1 #t)
(put! grid 2 2 #t)
(put! grid 3 0 #t)
(put! grid 3 1 #t)
(put! grid 3 2 #t)

;; Run for 80 iterations.
(life grid 80)
