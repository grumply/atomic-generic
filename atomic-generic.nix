{ mkDerivation, atomic, base, stdenv }:
mkDerivation {
  pname = "atomic-generic";
  version = "0.6.0.0";
  src = ./.;
  libraryHaskellDepends = [ atomic base ];
  homepage = "github.com/grumply/atomic-generic";
  description = "Generic rendering and parsing utilties.";
  license = stdenv.lib.licenses.bsd3;
}
