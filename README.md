# Tetris implemented in chicken scheme
build with `nix build` or install the dependencies with chicken manually. See `eggs`.  

# Controls
just the arrow keys (up to rotate clockwise)

# Peculiarities
* I have no clue how the Tetrominoes get shifted in regular Tetris. Really simple t spins work, though.
* The square can be rotated
* No score, no queue, but most of the abstractions for that are all there
* only rotates clockwise, no force drop
