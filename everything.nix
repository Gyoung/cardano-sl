{ mkDerivation, acid-state, aeson, aeson-options, aeson-pretty
, ansi-wl-pprint, array, asn1-encoding, asn1-types, async
, auto-update, base, base58-bytestring, base64-bytestring, basement
, beam-core, beam-migrate, beam-sqlite, bifunctors, binary
, bytestring, Cabal, canonical-json, cardano-crypto
, cardano-report-server, cborg, cereal, clock, concurrent-extra
, conduit, constraints, containers, contravariant, cryptonite
, cryptonite-openssl, data-default, data-default-class, deepseq
, deriving-compat, digest, directory, dlist, dns, ekg-core
, ekg-statsd, ekg-wai, ether, exceptions, extra, file-embed
, filelock, filepath, fmt, foldl, formatting, free
, generic-arbitrary, generics-sop, hashable, hourglass
, http-api-data, http-client, http-client-tls, http-conduit
, http-types, ip, iproute, ixset-typed, kademlia, katip, lens
, lifted-async, lrucache, lzma-conduit, megaparsec, memory, mmorph
, monad-control, mono-traversable, mtl, mwc-random
, neat-interpolation, network, network-info, network-transport
, network-transport-tcp, optparse-applicative, parsec, parsers
, plutus-prototype, process, pvss, QuickCheck, quickcheck-instances
, random, recursion-schemes, reflection, resourcet, retry
, rocksdb-haskell-ng, safe-exceptions, safecopy, scientific, scrypt
, semver, serokell-util, servant, servant-client
, servant-client-core, servant-generic, servant-multipart
, servant-server, servant-swagger, servant-swagger-ui
, servant-swagger-ui-core, servant-swagger-ui-redoc, silently
, sqlite-simple, sqlite-simple-errors, stdenv, stm
, streaming-commons, swagger2, systemd, tagged, tar
, template-haskell, text, th-utilities, these, time, time-units
, tls, transformers, transformers-base, transformers-lift, trifecta
, universum, unix, unliftio, unliftio-core, unordered-containers
, vector, wai, wai-middleware-throttle, wai-websockets, warp
, warp-tls, websockets, x509, x509-store, x509-validation, yaml
}:
mkDerivation {
  pname = "everything";
  version = "1.3.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    acid-state aeson aeson-options aeson-pretty ansi-wl-pprint array
    asn1-encoding asn1-types async auto-update base base58-bytestring
    base64-bytestring basement beam-core beam-migrate beam-sqlite
    bifunctors binary bytestring Cabal canonical-json cardano-crypto
    cardano-report-server cborg cereal clock concurrent-extra conduit
    constraints containers contravariant cryptonite cryptonite-openssl
    data-default data-default-class deepseq deriving-compat digest
    directory dlist dns ekg-core ekg-statsd ekg-wai ether exceptions
    extra file-embed filelock filepath fmt foldl formatting free
    generic-arbitrary generics-sop hashable hourglass http-api-data
    http-client http-client-tls http-conduit http-types ip iproute
    ixset-typed kademlia katip lens lifted-async lrucache lzma-conduit
    megaparsec memory mmorph monad-control mono-traversable mtl
    mwc-random neat-interpolation network network-info
    network-transport network-transport-tcp optparse-applicative parsec
    parsers plutus-prototype process pvss QuickCheck
    quickcheck-instances random recursion-schemes reflection resourcet
    retry rocksdb-haskell-ng safe-exceptions safecopy scientific scrypt
    semver serokell-util servant servant-client servant-client-core
    servant-generic servant-multipart servant-server servant-swagger
    servant-swagger-ui servant-swagger-ui-core servant-swagger-ui-redoc
    sqlite-simple sqlite-simple-errors stm streaming-commons swagger2
    systemd tagged tar template-haskell text th-utilities these time
    time-units tls transformers transformers-base transformers-lift
    trifecta universum unix unliftio unliftio-core unordered-containers
    vector wai wai-middleware-throttle wai-websockets warp warp-tls
    websockets x509 x509-store x509-validation yaml
  ];
  executableHaskellDepends = [
    aeson aeson-options ansi-wl-pprint base bytestring
    cardano-report-server directory filepath formatting lens
    lifted-async neat-interpolation optparse-applicative process
    safe-exceptions silently text time-units universum unix
    unordered-containers yaml
  ];
  license = stdenv.lib.licenses.mit;
}
