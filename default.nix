{ compiler ? "ghc821" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = new: old: rec {

              tlc =
                new.callPackage ./deps/tlc/tlc.nix { };

              ef =
                new.callPackage ./deps/ef/ef.nix { };

              ef-base =
                new.callPackage ./deps/ef-base/ef-base.nix { };

              atomic-base =
                new.callPackage ./deps/atomic-base/atomic-base.nix { };

              trivial =
                new.callPackage ./deps/trivial/trivial.nix { };

              atomic-types =
                new.callPackage ./deps/atomic-types/atomic-types.nix { };

              atomic-html =
                new.callPackage ./deps/atomic-html/atomic-html.nix { };

              atomic-generic =
                new.callPackage ./atomic-generic.nix { };

            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { ef = pkgs.haskell.packages.${compiler}.ef;
    ef-base = pkgs.haskell.packages.${compiler}.ef-base;
    atomic-types = pkgs.haskell.packages.${compiler}.atomic-types;
    atomic-base = pkgs.haskell.packages.${compiler}.atomic-base;
    tlc = pkgs.haskell.packages.${compiler}.tlc;
    trivial = pkgs.haskell.packages.${compiler}.trivial;
  }

