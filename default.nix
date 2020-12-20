{ pkgs ? import <nixpkgs> {}, hpkgs ? pkgs.haskellPackages, mkDerivation ? hpkgs.mkDerivation }:
hpkgs.callCabal2nix "bases" ./. { inherit mkDerivation; }
