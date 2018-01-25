{ mkDerivation, base, curl, fetchgit, scalpel, stdenv, text
, webdriver
}:
mkDerivation {
  pname = "financial-news-scraper";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/v0d1ch/financial-news-scraper.git";
    sha256 = "0bji3yn4lvpc2qs4q2rghrfy37q7kcfklzgjbaqxq0g1q4wx3pd7";
    rev = "8c11f4c76b55a143a8bf935fda2f4866f31ab551";
  };
  libraryHaskellDepends = [ base curl scalpel text webdriver ];
  homepage = "https://github.com/v0d1ch/financial-news-scraper#readme";
  license = stdenv.lib.licenses.bsd3;
}
