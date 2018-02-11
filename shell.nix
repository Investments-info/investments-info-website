{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  financialNewsScraper = import ../financial-news-scraper {};

  f = { mkDerivation, aeson, amazonka, amazonka-core, amazonka-ses
      , async, base, base16-bytestring, bcrypt, blaze-builder, byteable
      , bytestring, case-insensitive, Chart-diagrams, classy-prelude
      , classy-prelude-conduit, classy-prelude-yesod, conduit
      , conduit-extra, containers, cookie, cryptohash, csv, csv-conduit
      , data-default, directory, email-validate, errors, esqueleto
      , exceptions, fast-logger, file-embed, financial-news-scraper
      , foreign-store, generic-deriving, hashable, hjsmin, hspec
      , http-client, http-client-tls, http-conduit, http-types, lens
      , MailchimpSimple, microlens, monad-control, monad-logger, mtl
      , persistent, persistent-postgresql, persistent-sqlite
      , persistent-template, pretty-show, process, regex-pcre-builtin
      , safe, shakespeare, split, stdenv, template-haskell, text, time
      , transformers, unordered-containers, utf8-string, uuid, uuid-types
      , vector, wai, wai-extra, wai-logger, warp, warp-tls, wreq, yadata
      , yaml, yesod, yesod-auth, yesod-core, yesod-form, yesod-static
      , yesod-test, zlib, Cocoa, CoreServices
      }:
      mkDerivation {
        pname = "investments-info";
        version = "0.1.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson amazonka amazonka-core amazonka-ses async base
          base16-bytestring bcrypt blaze-builder byteable bytestring
          case-insensitive Chart-diagrams classy-prelude
          classy-prelude-conduit classy-prelude-yesod conduit conduit-extra
          containers cookie cryptohash csv csv-conduit data-default directory
          email-validate errors esqueleto exceptions fast-logger file-embed
          financial-news-scraper foreign-store generic-deriving hashable
          hjsmin http-client http-client-tls http-conduit http-types lens
          MailchimpSimple monad-control monad-logger mtl persistent
          persistent-postgresql persistent-sqlite persistent-template
          pretty-show process regex-pcre-builtin safe shakespeare split
          template-haskell text time transformers unordered-containers
          utf8-string uuid uuid-types vector wai wai-extra wai-logger warp
          warp-tls wreq yadata yaml yesod yesod-auth yesod-core yesod-form
          yesod-static zlib Cocoa CoreServices
        ];
        executableHaskellDepends = [
          aeson amazonka amazonka-core amazonka-ses async base
          base16-bytestring bcrypt blaze-builder byteable bytestring
          case-insensitive Chart-diagrams classy-prelude
          classy-prelude-conduit classy-prelude-yesod conduit conduit-extra
          containers cookie cryptohash csv csv-conduit data-default directory
          email-validate errors esqueleto exceptions fast-logger file-embed
          financial-news-scraper foreign-store generic-deriving hashable
          hjsmin http-client http-client-tls http-conduit http-types lens
          MailchimpSimple monad-control monad-logger mtl persistent
          persistent-postgresql persistent-sqlite persistent-template
          pretty-show process regex-pcre-builtin safe shakespeare split
          template-haskell text time transformers unordered-containers
          utf8-string uuid uuid-types vector wai wai-extra wai-logger warp
          warp-tls wreq yadata yaml yesod yesod-auth yesod-core yesod-form
          yesod-static zlib Cocoa CoreServices
        ];
        testHaskellDepends = [
          aeson amazonka amazonka-core amazonka-ses async base
          base16-bytestring bcrypt blaze-builder byteable bytestring
          case-insensitive Chart-diagrams classy-prelude
          classy-prelude-conduit classy-prelude-yesod conduit conduit-extra
          containers cookie cryptohash csv csv-conduit data-default directory
          email-validate errors esqueleto exceptions fast-logger file-embed
          financial-news-scraper foreign-store generic-deriving hashable
          hjsmin hspec http-client http-client-tls http-conduit http-types
          lens MailchimpSimple microlens monad-control monad-logger mtl
          persistent persistent-postgresql persistent-sqlite
          persistent-template pretty-show process regex-pcre-builtin safe
          shakespeare split template-haskell text time transformers
          unordered-containers utf8-string uuid uuid-types vector wai
          wai-extra wai-logger warp warp-tls wreq yadata yaml yesod
          yesod-auth yesod-core yesod-form yesod-static yesod-test
		  zlib Cocoa CoreServices
        ];
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
