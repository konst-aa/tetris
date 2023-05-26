{
  description = "Tetris written in chicken scheme!";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable"; # i like to live life dangerously
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      defaultBuildInputs =
        let
          stdenv = pkgs.stdenv;
          eggs = pkgs.chickenPackages.chickenEggs;
        in
        with eggs; [
          pkgs.chicken
          pkgs.makeWrapper
          pkgs.gnumake
          sdl2
          sdl2-ttf
          vector-lib
          srfi-123
        ];
    in
    {
      defaultPackage.x86_64-linux =
        pkgs.stdenv.mkDerivation {
          src = ./.;

          name = "chicken-tetris";
          buildInputs = defaultBuildInputs;
          buildPhase = ''
            make compile
          '';
          installPhase = ''
            mkdir -p $out/bin
            mkdir -p $out/etc
            cp -r fonts/ $out/etc/fonts
            cp out $out/bin/chicken-tetris

            for f in $out/bin/*
            do 
              wrapProgram $f \
               --set CHICKEN_REPOSITORY_PATH $CHICKEN_REPOSITORY_PATH \
               --set CHICKEN_TETRIS_FONTS $out/etc/fonts
            done
          '';
        };
      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs =
          defaultBuildInputs ++ [
            pkgs.egg2nix
          ];
      };
    };
}
