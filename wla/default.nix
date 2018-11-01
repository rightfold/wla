{ mkDerivation
, aeson
, base
, bytestring
, free
, http-client
, http-client-tls
, lens
, lens-aeson
, mtl
, text
, transformers
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
        free
        http-client
        http-client-tls
        lens
        lens-aeson
        mtl
        text
        transformers
        unordered-containers
    ];
}
