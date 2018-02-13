{ mkDerivation, aeson, base, bytestring, cairo, Chart, Chart-cairo
, Chart-diagrams, colour, containers, csv, http-client
, http-client-tls, http-conduit, lens, lens-aeson, mtl, process
, regex-pcre-builtin, stdenv, text, time, unordered-containers
, utf8-string, wreq
}:
mkDerivation {
  pname = "yadata";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring cairo Chart Chart-cairo Chart-diagrams colour
    containers csv http-client http-client-tls http-conduit lens
    lens-aeson mtl process regex-pcre-builtin text time
    unordered-containers utf8-string wreq
  ];
  executableHaskellDepends = [
    base bytestring csv lens regex-pcre-builtin time utf8-string wreq
  ];
  testHaskellDepends = [
    base bytestring csv lens regex-pcre-builtin time utf8-string wreq
  ];
  homepage = "https://github.com/qtests/yadata#readme";
  license = stdenv.lib.licenses.bsd3;
}
