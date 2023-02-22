(module grid
  (make-grid
    grid-x
    grid-y
    grid?
    grid-rows
    grid-cols
    grid-tile-width
    grid-color
    grid-tiles
    make-tile
    tile?
    tile-color
    tile-solid?
    gen-tiles
    render-grid!)

  (import scheme
          (chicken base)
          (vector-lib))

  (cond-expand
    (chicken-4 (use (prefix sdl2 "sdl2:")))
    (chicken-5 (import (prefix sdl2 "sdl2:"))))

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

  (define (gen-tiles rows cols)
    (vector-unfold
      (lambda (row)
        (values (vector-unfold
                  (lambda (col)
                    (values (make-tile '#(100 100 100 255) #f) (+ col)))
                  cols)))
      rows))

  (define (render-grid! renderer grid)
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

  )
