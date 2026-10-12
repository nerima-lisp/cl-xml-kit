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

The bootstrap loads the `cl-xml-kit/benchmark` ASDF system when available. It
also supports direct loading of the benchmark files, which is useful for
checking the harness before loading the full system. A complete run requires
the runtime dependency and the public parser API.

The controls are deterministic and reject values below the release standard:

| Variable | Default | Constraint |
| --- | ---: | --- |
| `BENCH_SEED` | `20261007` | decimal integer, at least 0 |
| `BENCH_WARMUP` | `2` | integer, at least 2 |
| `BENCH_SAMPLES` | `10` | integer, at least 10 |
| `BENCH_ITERATIONS` | none | compatibility alias for `BENCH_SAMPLES` |
| `BENCH_OPERATIONS` | `all` | `parse`, `utf8-parse`, `dom`, `pull`, `serialize`, or `all` |
| `BENCH_WORKLOADS` | `all` | workload IDs or `all` |

Workloads are `small`, `text-1mib`, `deep-512`, `attributes-128`,
`namespaces`, and `mixed-content`. The seed is included in generated payloads
and every shuffled round. Full GC occurs before, and never inside, a timed
section. Allocation is recorded with `sb-ext:get-bytes-consed`.

The TSV contains metadata, round order, and result rows. Result rows include
input bytes, sample count, median/minimum/maximum wall time, and allocation
bytes. Metadata includes source revision, dirty state, runtime, selection,
and source hashes.

## XMLConf

The catalog audit runs in the Nix flake check. To inspect a downloaded XMLConf
tree directly:

```sh
XML_KIT_XMLCONF_DIR=/path/to/xmlconf \
  sbcl --script scripts/xmlconf-runner.lisp --analyze
```

`--run` additionally executes selected valid, invalid, and not-wf cases. The
runner skips cases outside this implementation profile, including external
DTD/entity resolution, XML 1.1, and DTD validation declarations. Namespace
well-formedness errors remain fatal for the namespace-aware parser.

## Coverage and mutation

Coverage is an explicit release check:

```sh
sbcl --noinform --disable-debugger --script scripts/run-coverage.lisp
```

It requires non-empty source and test components, runs the same test system as
`run-tests.lisp`, writes LCOV output, and requires at least 90 percent
expression and branch coverage. Mutation checking requires a non-empty
selector and either `MUTATION_RUNNER` or `MUTATION_RESULTS`:

```sh
MUTATION_SELECTOR=xml-grammar,serializer-escape \
MUTATION_RUNNER=/path/to/mutation-runner \
sbcl --noinform --disable-debugger --script scripts/run-mutation.lisp
```

These scripts produce reports and do not modify source, test, ASDF, Nix, or
CI files.
