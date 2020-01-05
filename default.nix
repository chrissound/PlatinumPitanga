{
  nixpkgs ? (import (builtins.fetchTarball "https://github.com/input-output-hk/nixpkgs/archive/a8f81dc037a5977414a356dd068f2621b3c89b60.tar.gz") ( haskellnix ))
, haskellnix ? (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz))
} :
nixpkgs.haskell-nix.stackProject {
  src = nixpkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
}
