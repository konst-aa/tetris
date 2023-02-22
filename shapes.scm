(module shapes
  (make-shape
    shape?
    shape-points
    shape-offsets
    shape-rotation
    shape-color
    shapes-vec
    rot-points
    rotate-clockwise
    rotate-counterclockwise)
  (import scheme
          (chicken base)
          (vector-lib))

  (define-record-type shape
    (make-shape points offsets rotation color)
    shape?
    (points shape-points)
    (offsets shape-offsets)
    (rotation shape-rotation (setter shape-rotation)) ; 0 is spawn, 1 is right, 2 is 180, 3 is left
    (color shape-color))
  ;; https://tetris.wiki/Super_Rotation_System
  (define JLSTZ-offset
    `#(,(list (cons 0 0) (cons 0 0) (cons 0 0) (cons 0 0) (cons 0 0))
       ,(list (cons 0 0) (cons 0 1) (cons 1 1) (cons -2 0) (cons -2 1))
       ,(list (cons 0 0) (cons 0 0) (cons 0 0) (cons 0 0) (cons 0 0))
       ,(list (cons 0 0) (cons 0 -1) (cons 1 -1) (cons -2 0) (cons -2 -1))))

  (define I-offset
    `#(,(list (cons 0 0) (cons 0 -1) (cons 0 2) (cons 0 -1) (cons 0 2))
       ,(list (cons 0 -1) (cons 0 0) (cons 0 0) (cons -1 0) (cons 2 0))
       ,(list (cons -1 -1) (cons -1 1) (cons -1 -2) (cons 0 1) (cons 0 -2))
       ,(list (cons -1 0) (cons -1 0) (cons -1 0) (cons 1 0) (cons -2 0))))

  (define O-offset
    `#(,(list (cons 0 0))
       ,(list (cons 1 0))
       ,(list (cons 1 -1))
       ,(list (cons 0 -1))))

  (define J
    (make-shape (list (cons -1 -1) (cons 0 -1) (cons 0 0) (cons 0 1))
                JLSTZ-offset
                0
                #(24 7 250 255)))
  (define L
    (make-shape (list (cons 0 -1) (cons 0 0) (cons 0 1) (cons -1 1))
                JLSTZ-offset
                0
                #(250 133 7 255)))
  (define S
    (make-shape (list (cons 0 -1) (cons 0 0) (cons -1 0) (cons -1 1))
                JLSTZ-offset
                0
                #(72 250 7 255)))
  (define T
    (make-shape (list (cons 0 -1) (cons -1 0) (cons 0 0) (cons 0 1))
                JLSTZ-offset
                0
                #(116 34 240 255)))
  (define Z
    (make-shape (list (cons -1 -1) (cons -1 0) (cons 0 0) (cons 0 1))
                JLSTZ-offset
                0
                #(240 37 34 255)))
  (define I
    (make-shape (list (cons 0 -1) (cons 0 0) (cons 0 1) (cons 0 2))
                I-offset
                0
                #(85 221 242 255)))
  (define O
    (make-shape (list (cons -1 0) (cons 0 0) (cons -1 1) (cons 0 1))
                O-offset
                0
                #(236 240 34 255)))

  (define shapes-vec (list->vector (list J L S T Z I O)))

(define (nth-comp proc n)
  (define (helper acc n)
    (if (<= n 0)
      acc
      (helper (compose proc acc) (- n 1))))

  (helper identity n))
  (define (rot-points shape) ; rotated points
    (define (rot-90 p)
      (cons (cdr p) (- (car p))))
    (map (nth-comp rot-90 (shape-rotation shape))
         (shape-points shape)))

  (define (rotate-clockwise shape)
    (if (= (shape-rotation shape) 3)
      0
      (+ (shape-rotation shape) 1)))

  (define (rotate-counterclockwise shape)
    (if (= (shape-rotation shape) 0)
      3
      (- (shape-rotation shape) 1)))

  )
