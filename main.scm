(import (prefix sdl2 "sdl2:")
        (prefix sdl2-ttf "ttf:"))

(import (vector-lib)
        (chicken format)
        (chicken memory representation)
        (chicken process-context)
        (chicken random)
        (chicken time)
        shapes
        grid
        (srfi-123))

(sdl2:set-main-ready!)
(sdl2:init! '(video))
(ttf:init!)

(on-exit sdl2:quit!)

(define window (sdl2:create-window! "chicken-tetris" 100 100 450 620))
(define renderer (sdl2:create-renderer! window))
(define font (ttf:open-font
               (string-append (get-environment-variable "CHICKEN_TETRIS_FONTS")
                              "/ComicSansMS3.ttf") 30))

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


(define (point-op op . points)
  (cons (apply op (map car points ))
        (apply op (map cdr points))))

(define (any? alist)
  (foldl or #f alist))

(define (all? alist)
  (foldl and #t alist))

(define (stamp! shape color grid pos solidify?)
  (define (stamp-offset! offset)
    (let* ((real-row (+ (car pos) (car offset)))
           (real-col (+ (cdr pos) (cdr offset)))
           (tile (if (and (>= real-row 0)
                          (>= real-col 0)
                          (< real-row (grid-rows game-grid))
                          (< real-col (grid-cols game-grid)))
                   (~ (~ (grid-tiles grid) real-row) real-col)
                   #f)))
          (if tile
            (begin (set! (tile-color tile) color)
                   (if solidify?
                     (set! (tile-solid? tile) #t))))))
  (map stamp-offset! (rot-points shape)))

(define (intersecting? shape grid pos pos-offset)
  (define (bounds-check shape-offset)
    (let ((real-row (+ (car pos) (car shape-offset) (car pos-offset)))
          (real-col (+ (cdr pos) (cdr shape-offset) (cdr pos-offset))))
         (or (< real-col 0)
             (>= real-row (grid-rows grid))
             (>= real-col (grid-cols grid))
             (if (>= real-row 0) ; row < 0 won't be rendered out of bounds
               (tile-solid? (~ (~ (grid-tiles grid) real-row) real-col))
               #f))))
  (any? (map bounds-check (rot-points shape))))

(define (down! shape grid)
  (define placed (intersecting? shape grid cursor (cons 1 0)))

  ;remove previous render
  (stamp! current-shape (grid-color game-grid) game-grid cursor #f)

  (if placed
    (stamp! shape (shape-color shape) grid cursor #t)
    (begin
      (set! (car cursor) (+ (car cursor) 1))
      (stamp! current-shape (shape-color current-shape) game-grid cursor #f)))
  placed)

(define (placed-effects! grid cont)
  (define full-rows
    (vector-map
      (lambda (i row)
        (all? (vector->list (vector-map (lambda (j tile) (tile-solid? tile)) row))))
      (grid-tiles grid)))

  ; n^2 btw too lazy to impl the right way
  (vector-for-each
    (lambda (i full?)
      (if full?
        (begin
          (set! score (+ score 1))
          (set! (grid-tiles grid)
            (vector-concatenate
              (list
                (gen-tiles 1 10) ; insert at the top
                (vector-copy (grid-tiles grid) 0 i)
                (if (< (+ i 1) (grid-rows grid)) ; below removed layer
                  (vector-copy (grid-tiles grid) (+ i 1) (grid-rows grid))
                  #())))))))
    full-rows)

  ; loss check
  (if (any? (vector->list (vector-map
                            (lambda (_ tile) (tile-solid? tile))
                            (~ (grid-tiles grid) 0))))
    (cont "gg"))

  (set! current-shape (take-shape))
  (set! cursor (cons -1 5)))


(define game-grid
  (make-grid 10 10 20 10 30 #(100 100 100 255) (gen-tiles 20 10)))

(define cursor (cons -1 5))

(define (take-shape)
  (object-copy (~ shapes-vec (pseudo-random-integer (vector-length shapes-vec)))))

(define current-shape (take-shape))

(define (input-loop! cont)
  (define ev (sdl2:poll-event! main-event))
  (if (not ev)
    (cont 0))

  (define (try-offset shape offset)
    (not (intersecting? shape game-grid cursor offset)))

  (define (try-rotation! shape new-rot) ; bad code
    (let* ((old-rot (shape-rotation shape))
           (_ (set! (shape-rotation shape) new-rot))
           (res (foldl (lambda (success offset)
                         (cond
                           (success success)
                           ((try-offset current-shape offset)
                            (set! (shape-rotation shape) new-rot)
                            (set! cursor (point-op + cursor offset))
                            #t)
                           (else #f)))
                       #f
                       (map (lambda (p1 p2) (point-op - p1 p2))
                            (~ (shape-offsets shape)
                               old-rot)
                            (~ (shape-offsets shape)
                               new-rot)))))
          (if (not res)
            (set! (shape-rotation shape) old-rot))
          res))

  (stamp! current-shape (grid-color game-grid) game-grid cursor #f) ;remove previous render

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
                (if (try-offset current-shape (cons 0 -1))
                  (set! (cdr cursor) (- (cdr cursor) 1))))

               ((right)
                (if (try-offset current-shape (cons 0 1))
                  (set! (cdr cursor) (+ (cdr cursor) 1))))
               ((space)
                ((rec (force-drop!)
                      (let ((res (down! current-shape game-grid)))
                           (if res
                             (placed-effects! game-grid cont)
                             (force-drop!))))))
               ((up)
                (try-rotation! current-shape (rotate-clockwise current-shape)))

               ((x)
                (try-rotation! current-shape (rotate-clockwise current-shape)))

               ((z)
                (try-rotation! current-shape (rotate-counterclockwise current-shape))))))
  (stamp! current-shape (shape-color current-shape) game-grid cursor #f)

  ; render everything
  (render-grid! renderer game-grid)
  (sdl2:render-present! renderer)

  (input-loop! cont))

(define RENDERS-PER-SECOND 30)
(define MS-PER-RENDER (round (/ 1000 RENDERS-PER-SECOND)))

(define score 0)

;; increases with difficulty
(define renders-per-tick RENDERS-PER-SECOND)
(define render-count 1)
(define prev-render-time (current-process-milliseconds))

(define (with-control proc)
  (define res (call/cc (lambda (cont) (proc cont))))
  (cond
    ((equal? res "gg") (sdl2:quit!) (exit))))

(define (render-score! renderer score)
  (let* ((score-string (sprintf "score: ~A" score))
         (text-surf (ttf:render-utf8-shaded font
                                            score-string
                                            (sdl2:make-color 0 0 0)
                                            (sdl2:make-color 255 255 255))))
        (sdl2:render-copy! renderer
                           (sdl2:create-texture-from-surface renderer text-surf)
                           #f
                           (sdl2:make-rect 310 10
                                           (* 10 (length (string->list score-string)))
                                           40))))
(define prev-score-quotient 0)

(define (main)
  ; make frame length uniform
  (sdl2:delay! (- MS-PER-RENDER
                  (modulo (- (current-process-milliseconds) prev-render-time)
                          MS-PER-RENDER)))
  (set! prev-render-time (current-process-milliseconds))

  (with-control input-loop!) ;; respond to input

  (let* ((q (quotient score 10))
         (quotient-diff (- q prev-score-quotient)))
        (if (< q 30)
          (set! renders-per-tick (- renders-per-tick quotient-diff)))
        (set! prev-score-quotient q))

  ; force move down if the time is right
  (if (> render-count renders-per-tick)
    (begin
      (with-control
        (lambda (cont)
          (if (down! current-shape game-grid)
            (placed-effects! game-grid cont))
          (render-score! renderer score)
          (render-grid! renderer game-grid)
          (sdl2:render-present! renderer)))
      (set! render-count 1))
    (set! render-count (+ render-count 1)))

  (main))

(main)
(sdl2:quit!)
