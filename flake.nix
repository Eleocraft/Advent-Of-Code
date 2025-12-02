{
  description = "Haskell exs flake";

  inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
		flake-utils.lib.eachDefaultSystem (system:
			let
				pkgs = import nixpkgs { inherit system; };
				haskell = pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [
          stack
          haskell-language-server
        ]);
			in {
        devShells.default = pkgs.mkShell {
    		nativeBuildInputs = [ haskell ];
			};
		}
	);
}
