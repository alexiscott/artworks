{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";  # Stable release as of early 2024
    # or for slightly newer (but still stable) packages:
    # nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  };

  outputs = { self, nixpkgs, ... }@inputs: 
    let
      system = "x86_64-linux";  # or "aarch64-darwin" for M1 Mac
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          ghc
          cabal-install
          haskell-language-server
          hlint
          ormolu
        ];
      };
    };
}
