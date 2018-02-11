{ mkDerivation, base, bytestring, curl, safe-exceptions, scalpel
, stdenv, text, webdriver, xml-conduit
}:
mkDerivation {
  pname = "news-fetcher";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring curl safe-exceptions scalpel text webdriver
    xml-conduit
  ];
  homepage = "https://github.com/v0d1ch/financial-news-scraper#readme";
  license = stdenv.lib.licenses.bsd3;
}
