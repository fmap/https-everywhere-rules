let nixpkgs = (import <nixpkgs> {}).fetchgit {
  url = "git://github.com/nixos/nixpkgs.git";
  rev = "ef5c48326a7fd7f6d2dc944634afc6b55381dd6d";
  sha256 = "17kbhjgrpza61a2y8v1h21p8cmrdpvdajd0zrcb8vh18hw5pqa3i";
}; in { pkgs ? import nixpkgs {} }:
pkgs.haskellPackages.buildLocalCabal ./. "https-everywhere-rules-raw"
