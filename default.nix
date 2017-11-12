{pkgs ? import <nixpkgs> {}}:

pkgs.rustPlatform.buildRustPackage rec {
	name = "llang-${version}";
	version = "0.0.1";
	src = ./.;

	cargoSha256 = "1j5vf6iy0zjr3asj9ia1bf2rqdrzz9dzf7nv1jgg5ccxjc236mfq";

	buildInputs = [ pkgs.llvm_5 pkgs.rustfmt ];
}