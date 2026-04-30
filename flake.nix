{
  description = "A Haskell project";
  inputs.hix.url = "github:tek/hix?ref=0.9.1";
  outputs = {hix, ...}: hix.lib.flake {
    packages = {
      hie-bios = {
        src = ./.;
        description = "Set up a GHC API session and obtain flags required to compile a source file";
        cabal = {
          author = "Matthew Pickering <matthewtpickering@gmail.com>, Hannes Siebenhandl <fendor.haskell@gmail.com>";
          build-type = "Simple";
          license = "BSD-3-Clause";
          license-file = "LICENSE";
          version = "0.19.0";
          meta = {
            maintainer = "Hannes Siebenhandl <fendor.haskell@gmail.com>";
            homepage = "https://github.com/haskell/hie-bios";
            synopsis = "Set up a GHC API session";
          };
        };
        library = {
          enable = true;
          dependencies = [
            "aeson >=1.4.4 && <2.3"
            "base16-bytestring >=0.1.1 && <1.1"
            "bytestring >=0.10.8 && <0.13"
            "co-log-core ^>=0.3.0"
            "deepseq >=1.4.3 && <1.6"
            "exceptions ^>=0.10"
            "cryptohash-sha1 >=0.11.100 && <0.12"
            "directory >=1.3.0 && <1.4"
            "filepath >=1.4.1 && <1.6"
            "time >=1.8.0 && <1.16"
            "extra >=1.6.14 && <1.9"
            "prettyprinter ^>=1.6 || ^>=1.7.0"
            "ghc >=9.2.1 && <9.15"
            "transformers >=0.5.2 && <0.7"
            "temporary >=1.2 && <1.4"
            "template-haskell >=2.18 && <2.25"
            "text >=1.2.3 && <2.2"
            "unix-compat >=0.5.1 && <0.8"
            "unordered-containers >=0.2.9 && <0.3"
            "yaml >=0.10.0 && <0.12"
            "file-embed >=0.0.11 && <1"
            "conduit >=1.3 && <2"
            "conduit-extra >=1.3 && <2"
          ];
          source-dirs = "src";
          language = "Haskell2010";
          ghc-options = [
            "-Wall"
          ];
          component = {
            other-modules = [
              "Paths_hie_bios"
            ];
          };
        };
        executables.hie-bios = {
          dependencies = [
            "co-log-core"
            "directory"
            "filepath"
            "hie-bios"
            "optparse-applicative >=0.17.1 && <0.20"
            "prettyprinter"
          ];
          source-dirs = "exe";
          language = "Haskell2010";
          ghc-options = [
            "-Wall"
          ];
          component = {
            other-modules = [
              "Paths_hie_bios"
            ];
          };
        };
        tests.bios-tests = {
          dependencies = [
            "aeson"
            "co-log-core"
            "extra"
            "transformers"
            "tasty"
            "tasty-hunit"
            "tasty-expected-failure"
            "hie-bios"
            "filepath"
            "directory"
            "prettyprinter"
            "temporary"
            "text"
            "ghc"
          ];
          source-dirs = "tests/";
          main = "Main";
          language = "Haskell2010";
          ghc-options = [
            "-Wall"
          ];
          component = {
            other-modules = [
              "Utils"
              "BiosTests"
              "ParserTests"
            ];
          };
        };
      };

    };
    main = "hie-bios";
  };
}
