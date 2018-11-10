{ nixpkgs ? import ./nix/nixpkgs.nix {} }:
{
    gui = nixpkgs.callPackage ./gui {};
    wla = nixpkgs.haskellPackages.callPackage ./wla {};
}
