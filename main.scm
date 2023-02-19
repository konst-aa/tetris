;; Compatible with both CHICKEN 4 and CHICKEN 5.
(cond-expand
  (chicken-4 (use (prefix sdl2 "sdl2:")))
  (chicken-5 (import (prefix sdl2 "sdl2:"))))

(import (vector-lib)
        (chicken format)
        (chicken random)
        (srfi-123))

(sdl2:set-main-ready!)
(sdl2:init! '(video))

(on-exit sdl2:quit!)

(define window (sdl2:create-window! "Hello, World!" 100 100 450 620))
(define renderer (sdl2:create-renderer! window))
(define main-event (sdl2:make-event))

;; from reference https://gitlab.com/chicken-sdl2/chicken-sdl2-examples/-/blob/master/eggsweeper/eggsweeper.scm
;; Disable various irrelevant event types, to avoid wasted time and
;; memory garbage from handling them.
(set! (sdl2:event-state 'text-editing) #f)
(set! (sdl2:event-state 'text-input) #f)
(set! (sdl2:event-state 'mouse-wheel) #f)
(set! (sdl2:event-state 'finger-down) #f)
(set! (sdl2:event-state 'finger-up) #f)
(set! (sdl2:event-state 'finger-motion) #f)
(set! (sdl2:event-state 'multi-gesture) #f)
(set! (sdl2:event-state 'mouse-button-up) #f)
(set! (sdl2:event-state 'mouse-button-down) #f)
(set! (sdl2:event-state 'mouse-motion) #f)

(define-record-type grid
  (make-grid x y rows cols tile-width bg-color tiles)
  grid?
  (x grid-x) ;; absolute position on window may be mutable eventually
  (y grid-y)
  (rows grid-rows)
  (cols grid-cols)
  (tile-width grid-tile-width (setter grid-tile-width)) ;; shooould be good enough for resizing
  (bg-color grid-color)
  (tiles grid-tiles (setter grid-tiles)))

(define-record-type tile
  (make-tile color solid?)
  tile?
  (color tile-color (setter tile-color))
  (solid? tile-solid? (setter tile-solid?)))

(define-record-type shape
  (make-shape points offsets color)
  shape?
  (points shape-points)
  (offsets offsets)
  (rotation 0) ; 0 is spawn, 1 is right, 2 is 180, 3 is left
  (color shape-color))

(define (gen-tiles rows cols)
  (vector-unfold
    (lambda (row)
      (values (vector-unfold
                (lambda (col)
                  (values (make-tile '#(100 100 100 255) #f) (+ col)))
                cols)))
    rows))

(define (render-grid! grid)
  (define (rectangle-wrapper tile row col)
    (define width (grid-tile-width grid))
    (sdl2:make-rect (+ (grid-x grid) (* width col))
                    (+ (grid-y grid) (* width row))
                    width
                    width))
  (vector-map
    (lambda (i row-vec)
      (vector-map
        (lambda (j tile)
          (sdl2:render-draw-color-set! renderer (apply sdl2:make-color (vector->list (tile-color tile))))
          (sdl2:render-fill-rect! renderer (rectangle-wrapper tile i j)))
        row-vec))
    (grid-tiles grid)))

(define (rotate-90 shape) ; 90 deg clockwise
  (make-shape
    (map (lambda (point)
           (cons (cdr point) (- (car point))))
         (shape-points shape))
    (shape-color shape)))

(define (stamp! shape color grid row col solidify?)
  (define (stamp-offset! point)
    (let* ((real-row (+ row (car point)))
           (real-col (+ col (cdr point)))
           (tile (if (>= real-row 0)
                     (~ (~ (grid-tiles grid) real-row) real-col)
                     #f)))
          (if tile
              (begin (set! (tile-color tile) color)
                     (if solidify?
                         (set! (tile-solid? tile) #t))))))
  (map stamp-offset! (shape-points shape)))

(define (any? alist)
  (foldr or #f alist))

(define (all? alist)
  (foldr and #t alist))

(define (intersecting? shape grid row col)
  (define (bounds-check point)
    (let ((real-row (+ row (car point)))
          (real-col (+ col (cdr point))))
         (or (< real-col 0)
             (>= real-row (grid-rows grid))
             (>= real-col (grid-cols grid))
             (if (>= real-row 0) ; row < 0 won't be rendered out of bounds
                 (tile-solid? (~ (~ (grid-tiles grid) real-row) real-col))
                 #f))))
  (any? (map bounds-check (shape-points shape))))

(define (down! shape grid)
  (define placed (intersecting? shape grid (+ cursor-row 1) cursor-col))

  ;remove previous render
  (stamp! current-shape (grid-color game-grid) game-grid cursor-row cursor-col #f)

  (if placed
      (stamp! shape (shape-color shape) grid cursor-row cursor-col #t)
      (begin
        (set! cursor-row (+ cursor-row 1))
        (stamp! current-shape (shape-color current-shape) game-grid cursor-row cursor-col #f)))

  placed)

(define (placed-effects! grid cont)
  (define full-rows
    (vector-map
      (lambda (i row)
        (all? (vector->list (vector-map (lambda (j tile) (tile-solid? tile)) row))))
      (grid-tiles grid)))

  ; n^2 btw don't want to impl the intervals way of doing it
  (vector-for-each
    (lambda (i full?)
      (if full?
          (set! (grid-tiles grid)
                (vector-concatenate
                  (list
                    (gen-tiles 1 10) ; insert at the top
                    (vector-copy (grid-tiles grid) 0 i)
                    (if (< (+ i 1) (grid-rows grid)) ; below removed layer
                        (vector-copy (grid-tiles grid) (+ i 1) (grid-rows grid))
                        #()))))))
    full-rows)

  ; loss check
  (if (any? (vector->list (vector-map
                            (lambda (_ tile) (tile-solid? tile))
                            (~ (grid-tiles grid) 0))))
      (cont "gg"))

  (set! current-shape (take-shape))
  (set! cursor-row -1)
  (set! cursor-col 5))

(define game-grid
  (make-grid 10 10 20 10 30 #(100 100 100 255) (gen-tiles 20 10)))

(define bar
  (make-shape (list (cons 0 0) (cons 0 -2) (cons 0 -1) (cons 0 1))
              #(100 100 255 255)))
(define L
  (make-shape (list (cons 0 0) (cons 0 -1) (cons 0 1) (cons -1 -1))
              #(255 125 0 255)))

(define Z
  (make-shape (list (cons 0 0) (cons -1 0) (cons -1 1) (cons 0 -1))
              #(0 255 0 255)))

(define T
  (make-shape (list (cons 0 0) (cons 0 -1) (cons 0 1) (cons -1 0))
              #(255 0 255 255)))

(define box
  (make-shape (list (cons 0 0) (cons 0 1) (cons -1 0) (cons -1 1))
              #(255 255 0 255)))

(define shapes (list->vector (list bar L Z T box)))

(define cursor-row -1)
(define cursor-col 5)

(define (take-shape)
  (~ shapes (pseudo-random-integer (vector-length shapes))))

(define current-shape (take-shape))

(define (input-loop cont)
  (define ev (sdl2:poll-event! main-event))
  (if (not ev)
      (cont 0))

  (define (try-col shape col)
    (not (intersecting? shape game-grid cursor-row col)))

  (stamp! current-shape (grid-color game-grid) game-grid cursor-row cursor-col #f) ;remove previous render

  (case (sdl2:event-type ev)
        ;;((window)
        ;; (sdl2:update-window-surface! window))

        ;; User requested app quit (e.g. clicked the close button).
        ((quit)
         (begin (sdl2:quit!) (exit)))

        ;; Keyboard key pressed
        ((key-down)
         (case (sdl2:keyboard-event-sym ev)
               ((down)
                (if (down! current-shape game-grid)
                    (placed-effects! game-grid cont)))

               ((left)
                (if (try-col current-shape (- cursor-col 1))
                    (set! cursor-col (- cursor-col 1))))

               ((right)
                (if (try-col current-shape (+ cursor-col 1))
                    (set! cursor-col (+ cursor-col 1))))

               ((up)
                (let ((rotated (rotate-90 current-shape)))
                     ; tries a bunch of offsets
                     (foldl (lambda (success offset)
                              (cond
                                (success success)
                                ((try-col rotated (+ cursor-col offset))
                                 (set! current-shape rotated)
                                 (set! cursor-col (+ cursor-col offset)))
                                (else #f)))
                            #f
                            (list 0 -1 -2 1 2)))))))

  (stamp! current-shape (shape-color current-shape) game-grid cursor-row cursor-col #f)

  ; render everything
  (render-grid! game-grid)
  (sdl2:render-present! renderer)

  (input-loop cont))

(define RENDERS-PER-SECOND 30)
(define MS-PER-RENDER (round (/ 1000 RENDERS-PER-SECOND)))

(define score 0)

;; increases with difficulty
(define renders-per-tick RENDERS-PER-SECOND)
(define render-count 1)

(define (with-control proc)
  (define res (call/cc (lambda (cont) (proc cont))))
  (cond
    ((equal? res "gg") (sdl2:quit!) (exit))))

(define (main)
  (sdl2:delay! MS-PER-RENDER)

  (with-control input-loop) ;; respond to input

  ; force move down if the time is right
  (if (> render-count renders-per-tick)
      (begin
        (with-control
          (lambda (cont)
            (if (down! current-shape game-grid)
                (placed-effects! game-grid cont))
            (render-grid! game-grid)
            (sdl2:render-present! renderer)))
        (set! render-count 1))
      (set! render-count (+ render-count 1)))

  (main))

(main)
(sdl2:quit!)
