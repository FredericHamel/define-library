
(define-library (scheme thread)
  (namespace "")
  
  ;; Subset of thread functionality
  (export
    make-thread
    thread-name
    thread-recieve
    thread-send
    thread-start!
    thread-join!
    thread-sleep!))
