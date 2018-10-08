{
  allowUnfree = true;
  packageOverrides = pkgs : rec {
   ghcEnv = pkgs.haskellPackages.ghcWithHoogle
     (haskellPackages: with haskellPackages; [
       cabal-install
     ]);
  };
}
