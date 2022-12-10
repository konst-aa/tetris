;; Compatible with both CHICKEN 4 and CHICKEN 5.
(cond-expand
 (chicken-4 (use (prefix sdl2 "sdl2:")))
 (chicken-5 (import (prefix sdl2 "sdl2:"))))

(import (vector-lib)
        (chicken format))

(sdl2:set-main-ready!)
(sdl2:init! '(video))

(on-exit sdl2:quit!)

(define window (sdl2:create-window! "Hello, World!" 100 100 600 400))
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
  (make-grid x y width tiles)
  grid?
  (x grid-x) ;; absolute position on window may be mutable eventually
  (y grid-y)
  (width grid-width (setter grid-width)) ;; shooould be good enough for resizing
  (tiles grid-tiles))

(define-record-type tile
  (make-tile rel-x rel-y color render-flag)
  tile?
  (rel-x tile-rel-x) ;; top left corner!!
  (rel-y tile-rel-y) ;; note that a tile is rendered through a grid, so tile position is relative

  (color tile-color (setter tile-color))
  (render-flag tile-render-flag (setter tile-render-flag)))

(define-record-printer (tile t out)
  (fprintf out "#,(tile ~s ~s ~s ~s)"
  (tile-rel-x t) (tile-rel-y t) (tile-color t) (tile-render-flag t)))

(define (gen-tiles rows cols)
  (vector-unfold 
    (lambda (rel-y)
        (values (vector-unfold 
          (lambda (rel-x)
            (values (make-tile rel-x rel-y '#(255 0 0 255) #t) (+ rel-x)))
          cols)))
    rows))

(define (render-grid! grid)
  (define (rectangle-wrapper tile)
    (define width (grid-width grid))
    (sdl2:make-rect (+ (grid-x grid) (* width (tile-rel-x tile)))
                    (+ (grid-y grid) (* width (tile-rel-y tile)))
                    width
                    width))
  (vector-map 
    (lambda (i row-vec)
      (vector-map
        (lambda (j tile)
          (if (tile-render-flag tile)
            (sdl2:render-draw-color-set! renderer (apply sdl2:make-color (vector->list (tile-color tile))))
                                         (sdl2:render-fill-rect! renderer (rectangle-wrapper tile)))
          (set! (tile-render-flag tile) #f))
        row-vec))
    (grid-tiles grid)))


(define game-grid
  (make-grid 10 10 10 (gen-tiles 10 5)))

(display (grid-tiles game-grid))

(define (render-loop cont)
  (define curr-event (sdl2:poll-event! main-event))
  (if (sdl2:quit-event? curr-event)
    (begin (sdl2:quit!) (exit)))
  (if (not curr-event)
    (cont 0))
  (render-grid! game-grid)
  (render-loop cont)
  ;(display curr-event)
  )

(define RENDERS-PER-SECOND (round (/ 1000 60)))

(define score 0)
;; increases with difficulty
(define renders-per-tick 10)

(define (main)
  (sdl2:delay! RENDERS-PER-SECOND)
  ;(sdl2:fill-rect! (sdl2:window-surface window)
  ;               #f
  ;               (sdl2:make-color 0 128 255))
  ;(sdl2:update-window-surface! window)
  (sdl2:render-present! renderer)
  (call/cc (lambda (cont) (render-loop cont)))
  (main))

(main)
(sdl2:quit!)
