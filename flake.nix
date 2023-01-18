{
  description = "Tetris written in chicken scheme!";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
    {
      packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;
      defaultPackage.x86_64-linux = pkgs.hello;
      devShell.x86_64-linux = pkgs.mkShell {
        buildInputs = [
          pkgs.neovim
          pkgs.zsh
          pkgs.lsd
          pkgs.openssh
          pkgs.git
          pkgs.nodejs
        ]; # bloatware??? kekw
      };
    };
}
