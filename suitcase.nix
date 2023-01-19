{ pkgs ? import <nixpkgs> { } }: # a list of packages I want in all shells, even pure ones... Passed into devShell.
[
  # needs java
  pkgs.xclip
  pkgs.rnix-lsp
  pkgs.fzf
  pkgs.nix
  pkgs.neovim
  pkgs.zsh
  pkgs.lsd
  pkgs.openssh
  pkgs.git
  pkgs.nodejs
  pkgs.less
] # actual bloatware doe
