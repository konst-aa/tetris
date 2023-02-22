{ pkgs, stdenv }:
rec {
  inherit (pkgs) eggDerivation fetchegg;

  box = eggDerivation {
    name = "box-3.6.0";

    src = fetchegg {
      name = "box";
      version = "3.6.0";
      sha256 = "1wyy22mips3439zxxhaw9q45f41csy6y3cx2dbnd2qinzlzi3z1l";
    };

    buildInputs = [
      
    ];
  };

  matchable = eggDerivation {
    name = "matchable-1.1";

    src = fetchegg {
      name = "matchable";
      version = "1.1";
      sha256 = "084hm5dvbvgnpb32ispkp3hjili8z02hamln860r99jx68jx6j2v";
    };

    buildInputs = [
      
    ];
  };

  miscmacros = eggDerivation {
    name = "miscmacros-1.0";

    src = fetchegg {
      name = "miscmacros";
      version = "1.0";
      sha256 = "0n2ia4ib4f18hcbkm5byph07ncyx61pcpfp2qr5zijf8ykp8nbvr";
    };

    buildInputs = [
      
    ];
  };

  r7rs = eggDerivation {
    name = "r7rs-1.0.7";

    src = fetchegg {
      name = "r7rs";
      version = "1.0.7";
      sha256 = "0x1a9210r39i1fa15711fa3dqvidqqfq49zdhc10ibyjf796x1r8";
    };

    buildInputs = [
      matchable
      srfi-1
      srfi-13
    ];
  };

  sdl2 = eggDerivation {
    name = "sdl2-0.4.1";

    src = fetchegg {
      name = "sdl2";
      version = "0.4.1";
      sha256 = "0dz2z2s2kfalb9vjlf0vfb6y6pvpn4bsh7lry0m7xja5ssy27s7q";
    };

    buildInputs = [
      srfi-1
      pkgs.SDL2
    ];
  };
  sdl2-ttf = eggDerivation {
    name = "sdl2-ttf-0.2.0";

    src = fetchegg {
      name = "sdl2-ttf";
      version = "0.2.0";
      sha256 = "0rz4harq6vbkd49cfjzazy7iz183qmy35sx9j61hqzkrvsilrcaz";
    };

    buildInputs = [
      sdl2
      miscmacros
      pkgs.SDL2_ttf
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

  srfi-123 = eggDerivation {
    name = "srfi-123-0.2.1";

    src = fetchegg {
      name = "srfi-123";
      version = "0.2.1";
      sha256 = "0fz5pgljcm30h6wl6m1g5fxhrsh7pg97p8z76px2k51k3vh33k1b";
    };

    buildInputs = [
      srfi-99
      box
      r7rs
    ];
  };

  srfi-13 = eggDerivation {
    name = "srfi-13-0.3.3";

    src = fetchegg {
      name = "srfi-13";
      version = "0.3.3";
      sha256 = "09m424rwc76n9n9j8llhi70jjb47lfi2havpirq0rcvvgahfjwq7";
    };

    buildInputs = [
      srfi-14
    ];
  };

  srfi-14 = eggDerivation {
    name = "srfi-14-0.2.1";

    src = fetchegg {
      name = "srfi-14";
      version = "0.2.1";
      sha256 = "0gc33cx4xll9vsf7fm8jvn3gc0604kn3bbi6jfn6xscqp86kqb9p";
    };

    buildInputs = [
      
    ];
  };

  srfi-69 = eggDerivation {
    name = "srfi-69-0.4.3";

    src = fetchegg {
      name = "srfi-69";
      version = "0.4.3";
      sha256 = "11pny54nc3rpmpaxcxs9dap1n6490y80zpwgfg0bwji1938a6fks";
    };

    buildInputs = [
      
    ];
  };

  srfi-99 = eggDerivation {
    name = "srfi-99-1.4.5";

    src = fetchegg {
      name = "srfi-99";
      version = "1.4.5";
      sha256 = "18cpdkp46ksiv1yzmhfnjgrxgvz43xz03vk4dyrnldkc7i6xn4xz";
    };

    buildInputs = [
      srfi-1
      srfi-69
      miscmacros
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

