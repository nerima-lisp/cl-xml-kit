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

      # W5 XMLConf fetch block. W5 owns the archive input and its test derivation.
      extraOutputs =
        ctx:
        let
          xmlconfArchive = ctx.pkgs.fetchurl {
            url = "https://www.w3.org/XML/Test/xmlts20130923.tar.gz";
            sha256 = "9b61db9f5dbffa545f4b8d78422167083a8568c59bd1129f94138f936cf6fc1f";
          };
          xmlconfExclusions = ./scripts/xmlconf-exclusions.sexp;
          xmlconfCheck =
            ctx.pkgs.runCommand "cl-xml-kit-xmlconf-catalog"
              {
                nativeBuildInputs = [
                  ctx.pkgs.gnutar
                  ctx.pkgs.gzip
                  ctx.pkgs.sbcl
                ];
              }
              ''
                catalog_root="$TMPDIR/xmlconf"
                mkdir -p "$catalog_root"
                tar -xzf ${xmlconfArchive} -C "$catalog_root"
                export XML_KIT_XMLCONF_DIR="$catalog_root/xmlconf"
                export XML_KIT_XMLCONF_EXCLUSIONS="${xmlconfExclusions}"
                ${ctx.pkgs.sbcl}/bin/sbcl --noinform --disable-debugger \
                  --script "${ctx.src}/scripts/xmlconf-runner.lisp" > "$out"
                grep -Fx '  catalogs: 22' "$out"
                grep -Fx '  tests: 2585' "$out"
                grep -Fx '  accepted: 909' "$out"
                grep -Fx '  rejected: 948' "$out"
                grep -Fx '  skipped: 65' "$out"
                grep -Fx '  excluded: 663' "$out"
                grep -Fx '  output-cases: 376' "$out"
                grep -Fx '  unmatched-exclusions: 0' "$out"
                echo "XMLConf catalog check passed: 2585 catalog tests, 1857 selected cases"
              '';
        in
        {
          checks.xmlconf = xmlconfCheck;
          apps.bench = ctx.cl.mkTestApp {
            pname = "cl-xml-kit-bench";
            inherit (ctx) src;
            runner = "run-benchmarks.lisp";
            lispDependencies = [ cl-codec-kit.packages.${ctx.system}.cl-codec-kit ];
            description = "Run cl-xml-kit benchmarks";
          };
        };
      timeoutSeconds = 120;
      docs.root = ./docs;
      treefmt.evalModule = treefmt-nix.lib.evalModule;
    };
}
