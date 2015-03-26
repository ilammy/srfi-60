(define-library (bench)
  (export bench)

  (import (scheme base)
          (scheme inexact)
          (scheme time)
          (scheme write))

  (begin

    (define (time proc)
      (let* ((before (current-jiffy))
             (value (proc))
             (after (current-jiffy)))
        (- after before)))

    (define (gather-stats sample-count proc)
      (let loop ((count sample-count) (samples '()))
        (if (zero? count)
            samples
            (loop (- count 1)
                  (cons (time proc) samples)))))

    (define (bench name sample-count proc)
      (let ((samples (gather-stats sample-count proc)))
        (let ((average (average samples))
              (mininum (mininum samples))
              (maximum (maximum samples))
              (std-dev (std-dev samples)))
          (display name) (display ",")
          (display average) (display ",")
          (display mininum) (display ",")
          (display maximum) (display ",")
          (display std-dev) (newline))))

    (define (sum values)
      (apply + values))

    (define (average values)
      (/ (sum (map inexact values)) (length values)))

    (define (mininum values)
      (apply min values))

    (define (maximum values)
      (apply max values))

    (define (square x)
      (* x x))

    (define (std-dev values)
      (let ((avg (average values)))
        (sqrt (/ (sum (map (lambda (x) (square (- x avg))) values))
                 (length values)))))
))
