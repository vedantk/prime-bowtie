#lang racket

(require racket/draw)

(define (prime? n)
  (define (check-factors f limit)
    (cond
      [(>= f limit) #t]
      [(= (remainder n f) 0) #f]
      [else (check-factors (+ f 2) limit)]))
  (cond
    [(<= n 2) #f]
    [(= (remainder n 2) 0) #f]
    [else (check-factors 3 (integer-sqrt n))]))

(define (create-bowtie width height)
  (define target (make-bitmap width height))
  (define dc (new bitmap-dc% [bitmap target]))
  
  (define (set-font color)
    (send dc set-text-foreground color)
    (send dc set-font (make-object font% 9 'modern)))
  
  (set-font "black")
  (send dc draw-rectangle 0 0 width height)
  (send dc set-brush "white" 'solid)
  
  (define-values (char-w char-h _ char-above)
    (send dc get-text-extent "*"))
  
  (define char-vspace (* 2 (+ char-h char-above)))
  
  (define (column-space nums)
    (values (* (length nums) char-vspace)
            (* char-w (+ 2 1 (order-of-magnitude (last nums))))))
  
  (define (column-start vspace)
    (/ (- height vspace) 2))
  
  (define (emit-number k x0 y0)
    (when (prime? k)
      (set-font "red"))
    (send dc draw-text (number->string k) x0 y0)
    (set-font "black"))
  
  (define (emit-column nums x0 y0)    
    (for ([k nums]
          [i (in-range (length nums))])
      (emit-number k x0 (+ y0 (* i char-vspace)))))
  
  (define (generate-nums base-nums n)
    (let ([final (last base-nums)])
      (stream->list (in-range (+ 1 final) (+ 1 final n)))))
  
  (define-values (center-x center-y)
    (values (/ width 2) (/ height 2)))

  (define (draw-columns rhs-nums center-delta)
    (define-values (col-vspace col-hspace)
      (column-space rhs-nums))
    (define col-y0 (column-start col-vspace))
    (define col-len (length rhs-nums))
    (define lhs-nums (generate-nums rhs-nums col-len))
    
    (when (and (> col-y0 char-vspace)
               (> (- center-x center-delta) char-w))
      (emit-column rhs-nums (+ center-x center-delta) col-y0)
      (emit-column lhs-nums (- center-x center-delta) col-y0)
      
      (draw-columns (generate-nums lhs-nums (+ 1 col-len))
                    (+ col-hspace center-delta))))
  
  (draw-columns '(2 3) (* 2 char-w))
  
  (emit-number 1 center-x (- center-y (* 2 char-w)))
   
  target)

(create-bowtie 1000 600)
