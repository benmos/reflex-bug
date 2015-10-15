let
  sysPkgs = import <nixpkgs> {};
  reflexPkgs = import (sysPkgs.fetchgit {
    url = git://github.com/ryantrinkle/try-reflex;
    rev = "490e2ac4df8fbee5c19a1a5480f1d27fae201df0";
    sha256 = "ac028208dbc53acb39fc1228b3238b1e87ef1def2e322d652cb7a82bb1c8cdd4";
  }) {};
in
  reflexPkgs.ghcjs.callPackage ./. { }

