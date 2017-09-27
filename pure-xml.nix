{ mkDerivation, pure, base, stdenv }:
mkDerivation {
  pname = "pure-xml";
  version = "0.6.0.0";
  src = ./.;
  libraryHaskellDepends = [ pure base ];
  homepage = "github.com/grumply/pure-generic";
  description = "Generic rendering and parsing utilties.";
  license = stdenv.lib.licenses.bsd3;
}
