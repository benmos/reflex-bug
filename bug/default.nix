{ mkDerivation, stdenv,

  errors, feed, ghcjs-base, lens, mtl, network, network-uri, reflex, reflex-dom, text, time, transformers
}:

mkDerivation (
let
  lib         = stdenv.lib;
  isWithin    = p: dirPath: lib.hasPrefix (toString dirPath) (toString p);
  cabalFilter = path: type: (let pathBaseName = baseNameOf path; in
                               !(lib.hasSuffix "~" pathBaseName) &&
                               !(lib.hasSuffix "#" pathBaseName) &&
                               !(lib.hasPrefix "." pathBaseName) &&
                               (
                                   pathBaseName == "Bug.cabal"     ||
                                   pathBaseName == "LICENSE"       ||
                                   pathBaseName == "Setup.lhs"     ||
                                   isWithin path ./src             ||
                                   false
                               )
                            );
in {
  pname = "reflex-bug";
  version = "1.0.0";

  # See [https://github.com/jasper72/server/issues/65] and
  #     [https://github.com/NixOS/cabal2nix/issues/71] etc...
  # configureFlags = "--ghc-option=-lgcc_s"; # We're GHCJS - so assume we don't need this

  # We White-list the files to use to in 'src' in order to avoid
  # spurious rebuilds. See eg [https://github.com/NixOS/nixpkgs/issues/3112]
  src = builtins.filterSource cabalFilter ./.;
  buildDepends = [

    # Others
    errors feed ghcjs-base lens mtl network network-uri reflex reflex-dom text time transformers
  ];

  isLibrary = false;
  doHaddock = false;
  hyperlinkSource = false; # Don't waste time on Haddock
  enableSplitObjs = false;
  license = null;
})
