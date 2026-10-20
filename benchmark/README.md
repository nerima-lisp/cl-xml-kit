# cl-xml-kit benchmark and verification checks

The benchmark measures string parsing, UTF-8 parsing, pull-event iteration, and
serialization. It validates every selected case before timing and checks each
measured result afterward. The benchmark uses `cl-weave:measure` for timing.

## Running the benchmark

From the repository root:

```sh
nix run .#bench
```

The benchmark ASDF system depends on `cl-xml-kit`, `cl-weave`, and
`cl-host-kit`; `cl-xml-kit` depends on `cl-codec-kit`. The flake supplies these
dependencies for the command above.

The benchmark enforces minimums of 2 warmup rounds and 10 samples:

| Variable | Default | Constraint |
| --- | ---: | --- |
| `BENCH_SEED` | `20261007` | decimal integer, at least 0 |
| `BENCH_WARMUP` | `2` | integer, at least 2 |
| `BENCH_SAMPLES` | `10` | integer, at least 10 |
| `BENCH_OPERATIONS` | `all` | comma-separated `parse`, `utf8-parse`, `pull`, `serialize`, or `all` |
| `BENCH_WORKLOADS` | `all` | comma-separated workload IDs or `all` |

For a quick parser smoke run, select the small workload and the string parser:

```sh
BENCH_OPERATIONS=parse BENCH_WORKLOADS=small nix run .#bench
```

Workloads are `small`, `text-1mib`, `deep-512`, `attributes-128`, `namespaces`,
and `mixed-content`. UTF-8 parsing is measured only for `small`; pull-event
iteration is measured only for `mixed-content`. The seed changes the `small`,
`text-1mib`, `attributes-128`, and `mixed-content` payloads and controls shuffled
case order. The `deep-512` and `namespaces` payloads are fixed. A full garbage
collection runs before each measured sample, outside the timed section.
Allocation is recorded with `sb-ext:get-bytes-consed`.

Output is plain text: one key-value summary followed by one key-value record per
sample. Each sample includes the case, operation, workload, input size, time in
milliseconds, and allocation bytes.

## XMLConf

The `xmlconf` flake check downloads the
[XML Test Suite 20130923 archive](https://www.w3.org/XML/Test/xmlts20130923.tar.gz),
runs every selected case, and compares the catalog counts and execution totals
(passed, failed, and skipped cases) with fixed values in `flake.nix`. For a
direct run, download the archive and extract it with
`tar -xzf xmlts20130923.tar.gz`. From the repository root, run the direct
runner in the Nix development shell, which provides its dependencies to ASDF.
Set `XML_KIT_XMLCONF_DIR` to the extracted `xmlconf` directory:

```sh
nix develop -c env XML_KIT_XMLCONF_DIR=/path/to/xmlconf \
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

`nix flake check` includes the expression and branch coverage gate. The check
uses `cl-weave` coverage statistics and writes native coverage data to
`coverage.dat` and an HTML report to `coverage-report/`. It enforces the
measured minimums configured in `scripts/run-coverage.lisp`. The form-level
accounting manifests are `scripts/coverage-exclusions.sexp` and
`scripts/coverage-follow-ups.sexp`. Each uncovered form must have exactly one
entry across the two files, with a non-empty reason. Stale or overlapping
entries fail the check.

The mutation runner uses `cl-weave:run-mutations` with its default operators on
the character-class predicates and `xml-digit-weight`. Its fixed targets are
`xml-char-p`, `xml-space-char-p`, `xml-name-start-char-p`, `xml-name-char-p`,
`xml-pubid-char-p`, `xml-ncname-start-char-p`, `xml-ncname-char-p`,
`xml-space-p`, `xml-ncname-p`, and `xml-digit-weight`. It runs the existing
focused tests for each function. Before mutation, it expands the loop forms.
The `LOOP ALWAYS` forms in `xml-space-p` and `xml-ncname-p` discard the `NIL`
returned when a predicate succeeds, so the runner removes those no-op branches
before applying mutations. Otherwise, mutating a discarded value could produce
an equivalent mutant that does not change behavior. A complete focused run with a failed
or errored test kills that mutant; an incomplete run, a surviving mutant, or a
mutation runner error fails the gate. Run it from the repository root in the
Nix development shell, which provides its dependencies to ASDF:

```sh
nix develop -c sbcl --noinform --disable-debugger --script scripts/run-mutation.lisp
```

The runner prints the total, killed, surviving, and errored mutant counts and
writes the native S-expression report to `mutation-results.sexp`.
