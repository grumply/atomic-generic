{ compiler ? "ghc821"
, secure ? false
, debugws ? false
, devel ? false
, debugapi ? false
}:

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

              trivial =
                new.callPackage ./deps/trivial/trivial.nix { };

              pure =
                new.callPackage ./deps/pure/pure.nix { secure=secure; debugws=debugws; devel=devel; debugapi=debugapi; };

              pure-xml =
                new.callPackage ./pure-xml.nix { };

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
    tlc = pkgs.haskell.packages.${compiler}.tlc;
    trivial = pkgs.haskell.packages.${compiler}.trivial;
    pure = pkgs.haskell.packages.${compiler}.pure;
    pure-xml = pkgs.haskell.packages.${compiler}.pure-xml;
  }

