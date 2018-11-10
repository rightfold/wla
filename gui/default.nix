{ lib, stdenv, gmp, ncurses5, zlib }:
let
    purescript =
        stdenv.mkDerivation rec {
            name = "purescript";
            src = fetchTarball {
                url = https://github.com/purescript/purescript/releases/download/v0.12.0/linux64.tar.gz;
                sha256 = "0b9qcicwmxl69gvwg20hpf1n2bmamdbbrr764y3wm75827xdmfs5";
            };
            buildInputs = [
                gmp
                ncurses5
                zlib
            ];
            dontStrip = true;

            dynamicLinker = stdenv.cc.bintools.dynamicLinker;
            libPath = lib.makeLibraryPath buildInputs;
            installPhase = ''
                mkdir -p "$out/bin"
                mv 'purs' "$out/bin/purs"
                patchelf                                                    \
                    --interpreter ${dynamicLinker}                          \
                    --set-rpath ${libPath}                                  \
                    "$out/bin/purs"
            '';
        };
in
    stdenv.mkDerivation {
        name = "gui";
        buildInputs = [
            purescript
        ];
        src = ./.;
        buildPhase = ''
            find 'src' -type f -name '*.purs' -print0 | xargs -0 purs compile
        '';
        installPhase = ''
            mkdir "$out"
            find 'output' -type f -name '*.js' -print0 |                    \
                xargs -0 purs bundle -m Main > "$out/gui.js"
            cp 'src/index.html' "$out/index.html"
        '';
    }
