{ pkgs, stdenv }:
rec {
  inherit (pkgs) eggDerivation fetchegg;

  sdl2 = eggDerivation {
    name = "sdl2-0.4.1";

    src = fetchegg {
      name = "sdl2";
      version = "0.4.1";
      sha256 = "0dz2z2s2kfalb9vjlf0vfb6y6pvpn4bsh7lry0m7xja5ssy27s7q";
    };

    buildInputs = [
      pkgs.SDL2
      srfi-1
    ];
  };

  srfi-1 = eggDerivation {
    name = "srfi-1-0.5.1";

    src = fetchegg {
      name = "srfi-1";
      version = "0.5.1";
      sha256 = "15x0ajdkw5gb3vgs8flzh5g0pzl3wmcpf11iimlm67mw6fxc8p7j";
    };

    buildInputs = [
      
    ];
  };

  vector-lib = eggDerivation {
    name = "vector-lib-2.1.1";

    src = fetchegg {
      name = "vector-lib";
      version = "2.1.1";
      sha256 = "1402p9vqlqx6a0a704wzbk3zbnq26m24l7li9xp54hfr555hqdfh";
    };

    buildInputs = [
      
    ];
  };
}

