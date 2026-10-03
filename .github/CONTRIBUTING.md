# Contributing

Guidance for working in this repository.

## What this repo is

`islandbrand` is an internal R package that applies Island Health branding to
ggplot2 figures, `flextable`/`gt` tables, and Quarto HTML and Word reports.
It is used by the PHASE team and by ISLH staff generally.

The brand assets it is derived from live in a separate repository,
`temuulene/islh-brand-standard`: the Adobe Illustrator logo sources, the
`.mhtml` captures of the brand standards site, and the Office templates. Only
the small assets this package actually needs are vendored into `inst/`.

## Git conventions

Conventional-commit subjects: `feat:`, `fix:`, `docs:`, `test:`, `chore:`,
`refactor:`. Breaking changes get `!`.

## Commands

```bash
Rscript -e 'devtools::document()'   # regenerate man/ and NAMESPACE
Rscript -e 'devtools::test()'       # run the test suite
Rscript -e 'devtools::check()'      # full R CMD check
air format .                        # format before committing (air >= 0.4)
```

## The constraint that drives the design

Island Health laptops have no compiler, and group policy blocks programs run
from a user library. Two consequences, both non-negotiable:

- **Never add a `src/` directory.** The package must install from source
  without a compiler.
- **Never use `pak` or any installer that unpacks with its own helper binary**
  (`pak`'s bundled `cmdunzip.exe` is blocked by policy). `islh_install_deps()`
  uses base R's `install.packages(type = "binary")`. A test enforces this.

## Dependencies

`Imports` holds only what every output format needs: `cli`, `ggplot2`,
`rlang`, `scales`, `systemfonts`, and the base packages. Everything format-specific
stays in `Suggests` behind `requireNamespace(..., quietly = TRUE)` guards.

**Dependency checks are format-specific.** HTML setup must not require the Word
packages (`flextable`, `officer`), and Word setup must not require `gt`. A
staff member writing HTML reports should never be made to install the Word
stack. Tests enforce both directions.

`ggplot2 (>= 3.5.0)` is a real floor, not caution: the discrete scales call
`ggplot2::discrete_scale()` without `scale_name`, which was a required argument
until 3.5.0.

## Design rules

- **Loading must stay inert.** `library(islandbrand)` may only define objects.
  No font probing, no `options()`, no messages, no `theme_set()`. `.onLoad()`
  initialises one empty state slot and nothing else. All active setup belongs in
  `islh_setup()`. A test asserts this.
- **Never use `<<-`.** A package namespace is locked at load time, so
  super-assignment fails at runtime. Mutable state lives in `.islh_state`, the
  environment created in `R/aaa-state.R`.
- **Do not compute constants at the top level of a file** if they call other
  package functions — that makes the build depend on file collation order. Use
  a zero-argument function with a `.islh_state` cache instead (see
  `.islh_pal_map()`).
- **The prefix is `islh_`, never `ih_`** — `ih` is Interior Health. A test
  greps for it.
- **Public API is dot-free; internals are `.islh_`-prefixed.** NAMESPACE is
  what actually hides internals now, so the prefix is a readability
  convention rather than a mechanism — but keep it, so a reader can tell at a
  glance whether a function is part of the taught API. If you export something
  new, add it to `islh_help()` and to `_pkgdown.yml`; a test snapshots the
  export list.
- Errors, warnings and messages route through `.islh_abort()`, `.islh_warn()`
  and `.islh_inform()` so condition classes stay consistent. An internal
  helper that can abort takes `call = rlang::caller_env()` and passes it to
  `.islh_abort()`, so the error names the exported function the user called.
  A test checks this.

## Style

Follow the tidyverse style guide and format with `air` before committing:
`snake_case`, `<-` for assignment, native `|>` where it helps readability,
explicit `package::function()` calls for anything outside base R. Do not add
tidyverse dependencies beyond `ggplot2`.

Roxygen docs on every exported function, wrapped at 80 characters; internals
get `@noRd`. Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`.

## Generated artifacts

Edit the generator, never the output — hand edits are lost on the next build.

### `inst/quarto/_extensions/islh/islh-report/islh-report-reference.docx`

Built by `data-raw/build_reference_docx.py` (Python 3 stdlib, needs `pandoc`).
It starts from `pandoc --print-default-data-file reference.docx` so every style
Pandoc actually emits exists, swaps in the official ISLH Office theme from
`data-raw/theme1.xml` (lifted verbatim from the letterhead template), and
applies style overrides on top.

Gotchas found the hard way, all handled — don't undo them:

- The script needs `pandoc` and Python's `lxml`.
- Pandoc's reference doc *used* to ship an empty self-closing `<w:sectPr/>`.
  Since pandoc 3.8 it ships a populated one carrying footnote settings. The
  script handles both, and carries `w:footnotePr`/`w:endnotePr` across rather
  than dropping them. It prints a note if a future pandoc puts something else
  in there, so a silent change gets noticed.
- The output tracks whichever pandoc built it. Pandoc 3.1 omits the
  `FootnoteBlockText` style that later versions emit, so build with a current
  pandoc or the reference doc quietly loses styles.
- `Subtitle` is `basedOn` `Title`, so it inherits bold/kerning/colour unless
  explicitly reset.
- Paragraph tab stops *merge* with the style's rather than replacing them, so a
  single `<w:tab/>` lands on the centre stop, not the right one. The footer
  steps through two.
- Tab positions derive from the real text width (9404 twips at the letterhead's
  1418-twip margins), not Word's 1"-margin defaults of 4680/9360.

To verify a change, render and look at it rather than trusting the XML:

```bash
pandoc test.md -o out.docx --reference-doc=inst/quarto/_extensions/islh/islh-report/islh-report-reference.docx
libreoffice --headless --convert-to pdf out.docx
pdftoppm -png -r 100 out.pdf page      # then read the PNG
```

BC Sans will not be installed in a sandbox, so rendered text there shows a
substituted font. That is expected, not a bug in the template.

### `inst/brand/tokens.json`

Built by `data-raw/sync-brand-tokens.R` from `.islh_colours` in `R/colours.R`,
which is the single source of truth for brand hexes. `inst/brand/_brand.yml`
and the Word reference doc both derive from it. `test-brand-tokens.R` fails if
the three drift apart.

## Versioning

`DESCRIPTION`'s `Version:` is the only version string; `islh_version()` reads
it. Bumping a version means editing `DESCRIPTION` and adding a `NEWS.md`
heading. Order NEWS entries alphabetically by function name.

## Prose

The README, vignettes and release notes are written for Island Health staff:
Canadian spelling, plain language, no marketing tone.
