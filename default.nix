{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, mtl, stdenv, random, lhs2tex, multiset, finitary, language-dot }:
      mkDerivation {
        pname = "vitrea";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base mtl stdenv random lhs2tex multiset finitary language-dot ];
        homepage = "https://github.com/mroman42/vitrea";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
