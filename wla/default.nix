{ mkDerivation
, aeson
, async
, base
, blaze-html
, bytestring
, contravariant
, exceptions
, free
, http-client
, http-client-tls
, http-types
, lens
, lens-aeson
, mmorph
, mtl
, pipes
, profunctors
, scientific
, semigroupoids
, text
, transformers
, unix
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
        async
        base
        blaze-html
        bytestring
        contravariant
        exceptions
        free
        http-client
        http-client-tls
        http-types
        lens
        lens-aeson
        mmorph
        mtl
        pipes
        profunctors
        scientific
        semigroupoids
        text
        transformers
        unix
        unordered-containers
        wai
        warp
    ];
}
