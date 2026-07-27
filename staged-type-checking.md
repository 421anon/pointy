# Staged Type Checking for Pointy Workflow Steps

## Motivation

Pointy pipelines compose steps into DAGs. Each step is an instance of a template (alignment, script, prediction model, etc.) with typed inputs and outputs. Currently the type system is implicit:

- **Input constraints** (`allowedTypes`): coarse string tags ("alignment", "dataSource") checked at Nix eval time.
- **Output tags** (`passthru.meta.pointy.type`): a string written into derivation metadata after build.
- **Extras** (`csvExtras`, `fastqExtras`): post-build derivations that scan actual output files and emit `meta.json` with discovered columns, types, and nullability.

Gaps:

1. No output type *declaration* before build — templates don't promise what they produce.
2. No structural matching — a consumer that needs an `editseq` column can only check that the upstream step is "some kind of script", not that it actually emits that column.
3. No contract verification — the extras system *discovers* but doesn't *enforce*.
4. No input type introspection — a template's build function can't query what columns its upstream step declared.

## The Two-Phase Model

Nix evaluation splits naturally into two phases:

| Phase | Nix term | Type-system role |
|---|---|---|
| Eval | Pure evaluation, produces `.drv` | **Type checking** — declared types are compared; structural containment (`⊇`) is decided |
| Build | `.drv` realisation, I/O happens | **Contract checking** — actual output is verified against the declared type |

### Phase 1: Eval (type checking)

Templates declare their output shape. Steps declare their input requirements. The compiler checks structural containment: for each edge $A \to B$, does $\text{declared}(A) \supseteq \text{required}(B)$?

This is pure, structural, and finishes before any derivation is built. A mismatch fails at authoring time.

Example: step `pridict` requires its input to have columns `{editseq, ref, spacer, rtt}`. Step `flanks` declares output columns `{sequence_name, editseq, ref, spacer, rtt, pbs_variant, target}`. Check: `declared ⊇ required` → pass.

### Phase 2: Build (contract checking)

After a step builds, a **contract guard** runs. This is a Nix derivation that:

1. Depends on the step's output.
2. Inspects the actual files (DuckDB `sniff_csv` for CSVs, line-count validation for FASTQs).
3. Compares against the declared type.
4. **If the contract is satisfied:** the guard produces a success marker (an empty derivation or a signed manifest). Downstream steps proceed.
5. **If the contract is violated:** the guard derivation fails. Nix's topological build order ensures no downstream step ever sees the invalid output — the failure poisons the subgraph.

## The Contract Guard Pattern

The guard is enforced by the **consumer**, not the producer. When step B declares that it requires input of shape $T$, the system inserts a guard derivation between A and B:

```
Step A builds → output at /nix/store/<hashA>
                      │
                      ▼
              ┌───────────────┐
              │ Contract Guard │  ← inserted by compiler
              │ checks A ⊇ T  │
              └───────┬───────┘
                      │ success
                      ▼
              Step B builds
```

### Caching the Guard Result

The guard is a pure function of A's output. Its result can be **cached on A's output** as extended metadata:

```
/nix/store/<hashA>-stepname/
  ├── edit_flanks.csv          ← the actual output
  └── .pointy/
      └── contract.json        ← cached guard result
          {
            "checked": "2026-07-27T...",
            "contract": "Csv{editseq, ref, spacer, rtt, ...}",
            "satisfied": true,
            "columns": {
              "editseq": {"type": "string", "nullable": false},
              "ref":     {"type": "string", "nullable": false},
              ...
            }
          }
```

Subsequent consumers of A don't re-run the guard — they read `contract.json` and trust the cached result. This is sound because Nix's content-addressing guarantees that the same store path always has the same contents. The cached contract becomes part of A's *effective type*.

If a new consumer requires a stricter contract (e.g., an additional column not previously checked), a new guard runs for that contract. The result is **merged** into the cache — the union of all satisfied contracts is recorded.

## Dependent Type Formulation

The system can be expressed in dependent type theory. The core constructs:

### Promise (deferred Σ-type)

```
Promise (x : T ** P x)
```

The type part ($T$, $P$'s shape) is checked at eval time. The witness ($x$) and proof ($P\;x$) are deferred to build time. A file upload is:

```
uploaded : Promise (f : File ** extension f = ".csv")
```

### Structural containment

```
contains : OutputType → OutputType → Dec ()
```

Pure, decidable at eval time. Compares declared column names and types; returns `Yes ()` if the declared type contains at least the required fields.

### Content-addressed step references

```
record Step where
  hash   : String       -- /nix/store/<hash>-...
  output : OutputType   -- declared output type
```

Steps are referenced by content hash, not by function application. The pipeline is a Merkle-DAG of types: the type of step B includes the hash of step A, recursively.

## Idris Formulation

The structural type machinery (column containment, template constraints, declared output types) is valid Idris. The phase distinction is not — Idris evaluates everything at compile time. Two approaches bridge this:

### Approach 1: Staging annotations

Extend the type theory with phase annotations (`@eval`, `@build`) and a `Promise` type constructor. This requires changes to Idris's core language.

### Approach 2: Metaprogramming (Program 1 → Program 2)

**Program 1** (runs at authoring time, before files exist):

1. Reads step declarations and template types.
2. Checks structural containment on declared types.
3. Emits **Program 2** — an Idris program whose record types ARE the contracts.

**Program 2** (runs inside the Nix build, files exist):

1. For each step, a record type declares the output columns as fields.
2. A constructor function reads the actual file and returns `Either String Record` — failing if columns are missing.
3. A downstream step's build function takes the upstream record as an argument — **the type of the function IS the proof that the contract is satisfied.**

```idris
-- Generated record for step flanks:
record Flanks_1937 where
  constructor MkFlanks_1937
  editseq : String
  ref     : String
  spacer  : String
  -- ...

-- Generated build function for step pridict:
build_pridict_1939 : Flanks_1937 -> String -> IO Scores
```

If `flanks` didn't declare `editseq`, `Flanks_1937` wouldn't have that field, and `build_pridict_1939` wouldn't compile. The type error **is** the pipeline validation.

Program 1 is a metaprogram that writes Program 2. No annotations, no type theory extensions — just one Idris program emitting another, with Nix scheduling the phases.

## Surface DSL

The metaprogramming architecture is compressed into a domain language:

```
pipeline "PBS Lib target flanks" {

  upload reads : DataSource { file: *.fastq.gz }
  upload lib   : Library    { file: *.csv }

  step aln : Alignment {
    library:    lib
    dataSource: reads
  }

  step flanks : Script {
    body: "python3 format_edit_flanks.py"
    deps: [aln, lib]
  } emits {
    sequence_name: text
    editseq:        text
    ref:            text
    spacer:         text
    rtt:            text
    pbs_variant:    text
    target:         text
  }

  step pridict : PridictPredict {
    editSequences: flanks    -- checked: flanks.emits ⊇ {editseq, ...}
    cellType:      "HEK"
  }
}
```

Four constructs:

| Construct | Expresses |
|---|---|
| `upload X : T { file: *.ext }` | Σ-type with deferred witness |
| `step X : T { inputs }` | Π-type with structural input constraints |
| `{ column: type }` in `emits` | Output contract declaration |
| `pipeline "N" { ... }` | DAG with reference resolution and contract guard insertion |

## Summary

1. **Two phases:** eval (type checking on declared shapes) and build (contract checking on actual files).
2. **Contract guards** are inserted by the compiler between producer and consumer. They fail fast and poison downstream builds.
3. **Guard results are cached** on the producer's output, making repeated checks free and enabling incremental contract refinement.
4. **The type of a downstream build function IS the proof** that the contract is satisfied — no separate verification step.
5. The surface DSL hides the metaprogramming (Program 1 → Program 2) behind four intuitive constructs.
