{ mkDerivation
, aeson
, base
, bytestring
, lens
, lens-aeson
, text
, unordered-containers }:
mkDerivation {
    pname = "wla";
    version = "0.0.0.0";
    license = null;
    src = ./.;
    buildDepends = [
        aeson
        base
        bytestring
        lens
        lens-aeson
        text
        unordered-containers
    ];
}
