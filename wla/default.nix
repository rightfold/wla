{ mkDerivation
, aeson
, base
, blaze-html
, bytestring
, contravariant
, free
, http-client
, http-client-tls
, lens
, lens-aeson
, mtl
, semigroupoids
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
        blaze-html
        bytestring
        contravariant
        free
        http-client
        http-client-tls
        lens
        lens-aeson
        mtl
        semigroupoids
        text
        transformers
        unordered-containers
    ];
}
