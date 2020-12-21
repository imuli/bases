{ pkgs ? import <nixpkgs> {}, hpkgs ? pkgs.haskellPackages, mkDerivation ? hpkgs.mkDerivation }:
hpkgs.callCabal2nix "bases" (pkgs.lib.cleanSource ./.) { inherit mkDerivation; }
