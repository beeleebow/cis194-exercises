{ mkDerivation, base, hspec, stdenv }:
mkDerivation {
  pname = "stacktrace-cis194";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  homepage = "https://github.com/stacktracehq/cosmos/tree/master/training/CIS194";
  description = "Resources for working through CIS194 together";
  license = stdenv.lib.licenses.bsd3;
}