{ mkDerivation, base, binary, bytestring, containers, directory
, dlist, file-embed, filepath, gitrev, Glob, hashable, lca, lens
, lib, megaparsec, mtl, optparse-applicative, parser-combinators
, random, transformers, unordered-containers, utf8-string, vector
}:
mkDerivation {
  pname = "jass-hot-code-reload";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary bytestring containers directory dlist file-embed
    filepath gitrev Glob hashable lca lens megaparsec mtl
    optparse-applicative parser-combinators random transformers
    unordered-containers utf8-string vector
  ];
  license = lib.licenses.lgpl3Only;
}
