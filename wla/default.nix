{ mkDerivation
, aeson
, base
, blaze-html
, bytestring
, contravariant
, free
, http-client
, http-client-tls
, http-types
, lens
, lens-aeson
, mtl
, semigroupoids
, text
, transformers
, unordered-containers
, wai
, warp }:
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
        http-types
        lens
        lens-aeson
        mtl
        semigroupoids
        text
        transformers
        unordered-containers
        wai
        warp
    ];
}
