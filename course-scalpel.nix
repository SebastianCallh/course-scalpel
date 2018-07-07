{ mkDerivation, aeson, base, containers, either, ghcid, hlint
, hspec, HUnit, megaparsec, monad-logger, mtl, optparse-applicative
, parser-combinators, prettyprinter, QuickCheck
, quickcheck-arbitrary-adt, safe, scalpel, stdenv, text, time
, validation
}:
mkDerivation {
  pname = "course-scalpel";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers either hlint hspec HUnit megaparsec
    monad-logger mtl parser-combinators prettyprinter QuickCheck
    quickcheck-arbitrary-adt safe scalpel text validation
  ];
  executableHaskellDepends = [
    base containers monad-logger mtl optparse-applicative scalpel text
    time
  ];
  executableToolDepends = [ ghcid ];
  testHaskellDepends = [
    base hspec HUnit mtl QuickCheck scalpel text
  ];
  homepage = "https://github.com/sebastiancallh/course-scalpel#readme";
  license = stdenv.lib.licenses.bsd3;
}
