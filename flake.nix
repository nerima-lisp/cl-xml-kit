{
  description = "XML 1.0 and Namespaces toolkit for Common Lisp";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    cl-nix-forge = {
      url = "github:nerima-lisp/cl-nix-forge/v0.6.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.4.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cl-host-kit = {
      url = "github:nerima-lisp/cl-host-kit/v0.2.5";
      inputs = {
        cl-nix-forge.follows = "cl-nix-forge";
        cl-weave.follows = "cl-weave";
        nixpkgs.follows = "nixpkgs";
        treefmt-nix.follows = "treefmt-nix";
      };
    };

    cl-codec-kit = {
      url = "github:nerima-lisp/cl-codec-kit/v0.6.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cl-parser-kit = {
      url = "github:nerima-lisp/cl-parser-kit/v1.2.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      cl-nix-forge,
      cl-weave,
      cl-host-kit,
      cl-codec-kit,
      cl-parser-kit,
      treefmt-nix,
    }:
    let
      # The release exports Linux packages only, so build its ASDF system for
      # each platform supported by this flake.
      clHostKitFor =
        system:
        cl-nix-forge.lib.${system}.lispDerivation {
          pname = "cl-host-kit";
          version = "0.2.5";
          lispSystem = "cl-host-kit";
          src = cl-host-kit.outPath;
        };
    in
    cl-nix-forge.lib.x86_64-linux.mkPackageFlake {
      inherit self nixpkgs;

      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      pname = "cl-xml-kit";
      asd = ./cl-xml-kit.asd;
      root = ./.;

      meta = {
        description = "XML 1.0 and Namespaces toolkit for Common Lisp";
        homepage = "https://github.com/nerima-lisp/cl-xml-kit";
        license = nixpkgs.lib.licenses.mit;
        platforms = nixpkgs.lib.platforms.unix;
      };

      lispDependencies = ctx: [
        cl-codec-kit.packages.${ctx.system}.cl-codec-kit
        cl-parser-kit.packages.${ctx.system}.cl-parser-kit
      ];
      lispCheckDependencies = ctx: [
        cl-weave.packages.${ctx.system}.cl-weave
        (clHostKitFor ctx.system)
      ];

      sourceInclude = [
        ./scripts/xmlconf-exclusions.sexp
        ./scripts/coverage-exclusions.sexp
        ./scripts/coverage-follow-ups.sexp
        ./t/__snapshots__/snapshots.sexp
      ];

      extraOutputs =
        ctx:
        let
          xmlconfArchive = ctx.pkgs.fetchurl {
            url = "https://www.w3.org/XML/Test/xmlts20130923.tar.gz";
            sha256 = "9b61db9f5dbffa545f4b8d78422167083a8568c59bd1129f94138f936cf6fc1f";
          };
          xmlconfReport = "xmlconf-report.txt";
          xmlconfExpectedLines = [
            "  catalogs: 22"
            "  tests: 2585"
            "  accepted: 909"
            "  rejected: 948"
            "  skipped: 65"
            "  excluded: 663"
            "  output-cases: 376"
            "  unmatched-exclusions: 0"
            "  passed: 353"
            "  failed: 0"
            "  skipped: 1504"
          ];
          xmlconfCheck =
            (ctx.cl.mkCommandCheck {
              drv = ctx.package;
              name = "cl-xml-kit-xmlconf";
              command = [
                (ctx.pkgs.lib.getExe ctx.pkgs.sbcl)
                "--script"
                "scripts/xmlconf-runner.lisp"
                "--run"
                "--catalog-dir"
                "xmlconf-corpus/xmlconf"
                "--report"
                xmlconfReport
              ];
              artifacts = [ xmlconfReport ];
              validationCommands = map (
                line: "grep -Fx -- ${ctx.pkgs.lib.escapeShellArg line} ${xmlconfReport}"
              ) xmlconfExpectedLines;
              timeoutSeconds = 300;
              nativeBuildInputs = [
                ctx.pkgs.gnutar
                ctx.pkgs.gzip
              ];
            }).overrideAttrs
              (old: {
                preCheck = (old.preCheck or "") + ''
                  mkdir xmlconf-corpus
                  tar -xzf ${xmlconfArchive} -C xmlconf-corpus
                '';
              });

          coverageCheck = ctx.cl.mkCommandCheck {
            drv = ctx.package;
            name = "cl-xml-kit-coverage";
            command = [
              (ctx.pkgs.lib.getExe ctx.pkgs.sbcl)
              "--script"
              "scripts/run-coverage.lisp"
            ];
            artifacts = [
              "coverage.dat"
              "coverage-report"
            ];
            timeoutSeconds = 120;
          };

          parserKitLoadCheck = ctx.cl.mkCommandCheck {
            drv = ctx.package;
            name = "cl-xml-kit-parser-kit-load";
            command = [
              (ctx.pkgs.lib.getExe ctx.pkgs.sbcl)
              "--noinform"
              "--non-interactive"
              "--eval"
              "(require :asdf)"
              "--eval"
              ''
                (progn
                  (asdf:load-system "cl-xml-kit")
                  (let ((version
                          (asdf:component-version
                           (asdf:find-system "cl-parser-kit"))))
                    (unless (string= version "1.2.0")
                      (error "expected cl-parser-kit 1.2.0, got ~a" version))
                    (format t "cl-parser-kit-version=~a~%" version))
                  (dolist (name (list "MAKE-TOKENIZER-CURSOR"
                                      "TOKENIZER-CURSOR-NEXT"))
                    (multiple-value-bind (symbol status)
                        (find-symbol name "CL-PARSER-KIT")
                      (unless (and symbol (eq status :external) (fboundp symbol))
                        (error "missing external function: ~a" name))
                      (format t "export=~a status=~a fbound=~a~%"
                              name status (fboundp symbol))))
                  (format t "asdf-load-system=cl-xml-kit success~%"))
              ''
            ];
            timeoutSeconds = 120;
          };

          benchApp = ctx.cl.mkTestApp {
            pname = "cl-xml-kit-bench";
            inherit (ctx) src;
            runner = "run-benchmarks.lisp";
            lispDependencies = [
              cl-codec-kit.packages.${ctx.system}.cl-codec-kit
              cl-parser-kit.packages.${ctx.system}.cl-parser-kit
              cl-weave.packages.${ctx.system}.cl-weave
              (clHostKitFor ctx.system)
            ];
            description = "Run cl-xml-kit benchmarks";
          };
        in
        {
          checks.xmlconf = xmlconfCheck;
          checks.coverage = coverageCheck;
          checks.parser-kit-load = parserKitLoadCheck;
          apps.bench = {
            type = "app";
            program = benchApp.program;
            meta.description = "Run cl-xml-kit benchmarks";
          };
        };
      timeoutSeconds = 120;
      docs.root = ./docs;
      treefmt.evalModule = treefmt-nix.lib.evalModule;
    };
}
