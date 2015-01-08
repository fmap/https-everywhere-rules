let nixpkgs = (import <nixpkgs> {}).fetchgit {
  url = "git://github.com/nixos/nixpkgs.git";
  rev = "ef5c48326a7fd7f6d2dc944634afc6b55381dd6d";
  sha256 = "17kbhjgrpza61a2y8v1h21p8cmrdpvdajd0zrcb8vh18hw5pqa3i";
}; in { pkgs ? import nixpkgs {} }: let
  httpsEverywhereRulesRaw = import (pkgs.fetchgit {
    url = "git://github.com/fmap/https-everywhere-rules-raw.git";
    rev = "c90400b0d366caef5946995a3db6c16bfe3ef33d";
    sha256 = "fe6c4ff4e731110c8cb13b81000773fee97acdf05becea1bd4ce851beb303a3e";
    fetchSubmodules = true;
  }) { inherit pkgs; };
in pkgs.haskellPackages.buildLocalCabalWithArgs {
  name = "https-everywhere-rules"; src = ./.; 
  args = { inherit httpsEverywhereRulesRaw; };
} 
