{ nixpkgs ? import ./nix/nixpkgs.nix {} }:
{
    wla = nixpkgs.haskellPackages.callPackage ./wla {};
}
