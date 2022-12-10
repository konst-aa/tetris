{ nixpkgs ? <nixpkgs> }:
let
  pkgs = import nixpkgs {};
  stdenv = pkgs.stdenv;
  eggs = import ./eggs.nix { inherit pkgs stdenv; };
in
pkgs.eggDerivation {
  src = ./.;

  name = "konst-eggs";
  buildInputs = with eggs; [
    pkgs.gnumake
    sdl2
    vector-lib
  ];
  buildPhase = ''
    csc -raw test.scm
    echo hello
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp test $out/bin
  '';
}
