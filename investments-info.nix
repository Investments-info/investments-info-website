{ mkDerivation, aeson, amazonka, amazonka-core, amazonka-ses, async
, base, bcrypt, bytestring, case-insensitive, Chart-diagrams
, classy-prelude, classy-prelude-conduit, classy-prelude-yesod
, conduit, conduit-extra, containers, cookie, csv, csv-conduit
, data-default, directory, email-validate, errors, esqueleto
, exceptions, fast-logger, file-embed, financial-news-scraper
, foreign-store, generic-deriving, hashable, hjsmin, hspec
, http-client, http-client-tls, http-conduit, lens, MailchimpSimple
, microlens, monad-control, monad-logger, mtl, persistent
, persistent-postgresql, persistent-sqlite, persistent-template
, pretty-show, process, regex-pcre-builtin, safe, shakespeare
, split, stdenv, template-haskell, text, time, transformers
, unordered-containers, utf8-string, uuid, uuid-types, vector, wai
, wai-extra, wai-logger, warp, warp-tls, wreq, yadata, yaml, yesod
, yesod-auth, yesod-core, yesod-form, yesod-static, yesod-test
}:
mkDerivation {
  pname = "investments-info";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson amazonka amazonka-core amazonka-ses async base bcrypt
    bytestring case-insensitive Chart-diagrams classy-prelude
    classy-prelude-conduit classy-prelude-yesod conduit conduit-extra
    containers cookie csv csv-conduit data-default directory
    email-validate errors esqueleto exceptions fast-logger file-embed
    financial-news-scraper foreign-store generic-deriving hashable
    hjsmin http-client http-client-tls http-conduit lens
    MailchimpSimple monad-control monad-logger mtl persistent
    persistent-postgresql persistent-sqlite persistent-template
    pretty-show process regex-pcre-builtin safe shakespeare split
    template-haskell text time transformers unordered-containers
    utf8-string uuid uuid-types vector wai wai-extra wai-logger warp
    warp-tls wreq yadata yaml yesod yesod-auth yesod-core yesod-form
    yesod-static
  ];
  executableHaskellDepends = [
    aeson amazonka amazonka-core amazonka-ses async base bcrypt
    bytestring case-insensitive Chart-diagrams classy-prelude
    classy-prelude-conduit classy-prelude-yesod conduit conduit-extra
    containers cookie csv csv-conduit data-default directory
    email-validate errors esqueleto exceptions fast-logger file-embed
    financial-news-scraper foreign-store generic-deriving hashable
    hjsmin http-client http-client-tls http-conduit lens
    MailchimpSimple monad-control monad-logger mtl persistent
    persistent-postgresql persistent-sqlite persistent-template
    pretty-show process regex-pcre-builtin safe shakespeare split
    template-haskell text time transformers unordered-containers
    utf8-string uuid uuid-types vector wai wai-extra wai-logger warp
    warp-tls wreq yadata yaml yesod yesod-auth yesod-core yesod-form
    yesod-static
  ];
  testHaskellDepends = [
    aeson amazonka amazonka-core amazonka-ses async base bcrypt
    bytestring case-insensitive Chart-diagrams classy-prelude
    classy-prelude-conduit classy-prelude-yesod conduit conduit-extra
    containers cookie csv csv-conduit data-default directory
    email-validate errors esqueleto exceptions fast-logger file-embed
    financial-news-scraper foreign-store generic-deriving hashable
    hjsmin hspec http-client http-client-tls http-conduit lens
    MailchimpSimple microlens monad-control monad-logger mtl persistent
    persistent-postgresql persistent-sqlite persistent-template
    pretty-show process regex-pcre-builtin safe shakespeare split
    template-haskell text time transformers unordered-containers
    utf8-string uuid uuid-types vector wai wai-extra wai-logger warp
    warp-tls wreq yadata yaml yesod yesod-auth yesod-core yesod-form
    yesod-static yesod-test
  ];
  license = stdenv.lib.licenses.unfree;
}
