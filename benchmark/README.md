# cl-xml-kit benchmark and release checks

`benchmark/run.lisp` measures string and UTF-8 parsing, DOM construction,
pull-event iteration, and serialization. Each selected case is parsed and
validated before timing begins. Missing public entry points raise a benchmark
API error and cannot be reported as a zero result.

## Running the benchmark

From the repository root:

```sh
sbcl --noinform --disable-debugger --script run-benchmarks.lisp \
  > benchmark-results.tsv
```

The bootstrap loads the `cl-xml-kit/benchmark` ASDF system, so ASDF must be
able to find the runtime dependency `cl-codec-kit`. A load failure stops the
run.

`nix run .#bench` runs the same harness with the dependencies supplied by the
flake.

The controls are deterministic and reject values below the release standard:

| Variable | Default | Constraint |
| --- | ---: | --- |
| `BENCH_SEED` | `20261007` | decimal integer, at least 0 |
| `BENCH_WARMUP` | `2` | integer, at least 2 |
| `BENCH_SAMPLES` | `10` | integer, at least 10 |
| `BENCH_OPERATIONS` | `all` | comma-separated `parse`, `utf8-parse`, `dom`, `pull`, `serialize`, or `all` |
| `BENCH_WORKLOADS` | `all` | comma-separated workload IDs or `all` |
| `BENCH_REVISION` | `unknown` | source revision recorded in the metadata |
| `BENCH_SOURCE_STATE` | `unknown` | `clean`, `dirty`, or `unknown` |

The harness records only the revision and working-tree state its caller
passes. `nix run .#bench` sets both from the flake: a clean Git checkout gives
its commit and `clean`, a modified one gives its commit and `dirty`, and a
`path:` flake or a tree without `.git` gives `unknown` for both. For a manual
run, set them yourself, for example
`BENCH_REVISION="$(jj log --no-graph -r @- -T commit_id)" BENCH_SOURCE_STATE=clean`
when the working-copy change `@` is empty.

Workloads are `small`, `text-1mib`, `deep-512`, `attributes-128`,
`namespaces`, and `mixed-content`. The seed is included in generated payloads
and every shuffled round. Full GC occurs before, and never inside, a timed
section. Allocation is recorded with `sb-ext:get-bytes-consed`.

The TSV contains metadata, round order, and result rows. Result rows include
input bytes, sample count, median/minimum/maximum wall time, and allocation
bytes. Metadata includes the source revision and state, runtime, selection,
and MD5 digests of the benchmark sources.

## XMLConf

The `xmlconf` flake check runs every selected case and compares the catalog
counts and the execution totals (passed, failed, and skipped cases) with fixed
values in `flake.nix`. To inspect a downloaded XMLConf tree directly:

```sh
XML_KIT_XMLCONF_DIR=/path/to/xmlconf \
  sbcl --script scripts/xmlconf-runner.lisp --run
```

Without `--run`, the runner prints only the catalog counts. With `--run`, it
also parses the selected valid, invalid, and not-wf cases, prints each outcome
and the totals, and exits with status 1 when any case fails. `--report FILE`
writes the output to FILE instead of standard output. Each case is passed to
`parse-xml` as octets, so the parser detects UTF-8 and UTF-16 itself. The
runner skips cases outside this implementation profile, including external
DTD/entity resolution, XML 1.1, and DTD validation declarations. Namespace
well-formedness errors remain fatal for the namespace-aware parser. A case
that runs longer than `XML_KIT_XMLCONF_CASE_TIMEOUT` seconds (default 10) is
reported as failed. An unknown option is a usage error (exit status 2);
`--help` lists the options and environment variables.

## Coverage and mutation

Coverage is an explicit release check:

```sh
sbcl --noinform --disable-debugger --script scripts/run-coverage.lisp
```

It requires non-empty source and test components, runs the same test system as
`run-tests.lisp`, writes LCOV output, and requires at least 90 percent
expression and branch coverage.

Mutation checking requires either `MUTATION_RESULTS` naming an existing result
file, which takes precedence, or `MUTATION_RUNNER`. The script kills
`MUTATION_RUNNER` and fails when it runs longer than `MUTATION_RUNNER_TIMEOUT`
seconds (default 3600):

```sh
MUTATION_SELECTOR=xml-grammar,serializer-escape \
MUTATION_RUNNER=/path/to/mutation-runner \
sbcl --noinform --disable-debugger --script scripts/run-mutation.lisp
```

These scripts produce reports and do not modify source, test, ASDF, Nix, or
CI files.
