{ haskell, haskellPackages }:
haskell.lib.dontHaddock (
  haskell.lib.disableLibraryProfiling (
    haskellPackages.callCabal2nix "pointy-slurm-events" ./. { }
  )
)
