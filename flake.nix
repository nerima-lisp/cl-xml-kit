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

    cl-codec-kit = {
      url = "github:nerima-lisp/cl-codec-kit/v0.6.0";
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
      cl-codec-kit,
      treefmt-nix,
    }:
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

      lispDependencies = ctx: [ cl-codec-kit.packages.${ctx.system}.cl-codec-kit ];
      lispCheckDependencies = ctx: [ cl-weave.packages.${ctx.system}.cl-weave ];

      sourceInclude = [ ./scripts/xmlconf-exclusions.sexp ];

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

          benchApp = ctx.cl.mkTestApp {
            pname = "cl-xml-kit-bench";
            inherit (ctx) src;
            runner = "run-benchmarks.lisp";
            lispDependencies = [ cl-codec-kit.packages.${ctx.system}.cl-codec-kit ];
            description = "Run cl-xml-kit benchmarks";
          };
          # A path: flake carries neither rev nor dirtyRev, so the benchmark
          # reports the revision as unknown instead of guessing.
          benchSource =
            if self ? rev then
              {
                revision = self.rev;
                state = "clean";
              }
            else if self ? dirtyRev then
              {
                revision = ctx.pkgs.lib.removeSuffix "-dirty" self.dirtyRev;
                state = "dirty";
              }
            else
              {
                revision = "unknown";
                state = "unknown";
              };
          benchRunner = ctx.pkgs.writeShellApplication {
            name = "cl-xml-kit-bench";
            text = ''
              export BENCH_REVISION=${ctx.pkgs.lib.escapeShellArg benchSource.revision}
              export BENCH_SOURCE_STATE=${benchSource.state}
              exec ${benchApp.program} "$@"
            '';
          };
        in
        {
          checks.xmlconf = xmlconfCheck;
          apps.bench = {
            type = "app";
            program = ctx.pkgs.lib.getExe benchRunner;
            meta.description = "Run cl-xml-kit benchmarks";
          };
        };
      timeoutSeconds = 120;
      docs.root = ./docs;
      treefmt.evalModule = treefmt-nix.lib.evalModule;
    };
}
