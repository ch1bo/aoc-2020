{ pkgs ? import <unstable> { }
, compilerVersion ? "8102" # NOTE: update this manually according to stack resolver
, compiler ? "ghc${compilerVersion}"
}:

with pkgs;
let
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: [
    ps.extra
    ps.safe
    ps.attoparsec
  ]);
  hls = pkgs.haskell-language-server.override
    { supportedGhcVersions = [ compilerVersion ]; };
in
pkgs.mkShell rec {
  buildInputs = [
    ghc
    hls
  ];
}
