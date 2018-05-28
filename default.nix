{ mkDerivation, base, pure-core, pure-txt, stdenv }:
mkDerivation {
  pname = "pure-xml";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-core pure-txt ];
  homepage = "github.com/grumply/pure-generic";
  description = "Generic XML rendering and parsing utilties";
  license = stdenv.lib.licenses.bsd3;
}
