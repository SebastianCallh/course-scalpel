{ mkDerivation, aeson, base, containers, either, hlint, hspec
, HUnit, megaparsec, monad-logger, mtl, optparse-applicative
, parallel, parser-combinators, prettyprinter, QuickCheck
, quickcheck-arbitrary-adt, safe, scalpel, split, stdenv, text
, time, turtle, validation
}:
mkDerivation {
  pname = "course-scalpel";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers either hlint hspec HUnit megaparsec
    monad-logger mtl parallel parser-combinators prettyprinter
    QuickCheck quickcheck-arbitrary-adt safe scalpel text turtle
    validation
  ];
  executableHaskellDepends = [
    aeson base containers monad-logger mtl optparse-applicative
    prettyprinter scalpel split text time
  ];
  testHaskellDepends = [
    base hspec HUnit mtl QuickCheck scalpel text validation
  ];
  homepage = "https://github.com/sebastiancallh/course-scalpel#readme";
  license = stdenv.lib.licenses.bsd3;
}
