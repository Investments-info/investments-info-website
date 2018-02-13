{ compiler ? "ghc822"
, withHoogle ? true
}:

let
  bootstrap = import <nixpkgs> {};
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };
  pkgs = import src {};
  f = import ./default.nix;
  packageSet = pkgs.haskell.packages.${compiler};
  hspkgs = (
    if withHoogle then
      packageSet.override {
        overrides = (self: super: {
          ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
          ghcWithPackages = self.ghc.withPackages;
        });
      }
      else packageSet
  );
  drv = hspkgs.callPackage f {};
in
  if pkgs.lib.inNixShell then drv.env else drv
