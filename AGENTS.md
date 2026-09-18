# AGENTS.md — Rfits project memory

Guidance for AI assistants working in this repository. Read this before editing code.

## What this is

`Rfits` is an R package (LGPL-3) providing mid- to high-level read/write access to
astronomy FITS files: images, vectors, cubes, 4D arrays, tables, and headers. It is
maintained by Aaron Robotham (asgr on GitHub) and lives at https://github.com/asgr/Rfits.
Current version is tracked in `DESCRIPTION` (also update the `Date:` field when bumping it).

The heavy lifting is in C++ (`src/Rfits.cpp`) via Rcpp against a **bundled, statically
built copy of cfitsio** in `src/cfitsio/`.

## Build and install

```r
# After any change to src/, install from the package root:
install.packages('.', repos=NULL, type='source')
# or, matching the Rproj settings:
# R CMD INSTALL --no-multiarch --with-keep.source .
```

- `configure` runs `src/cfitsio/configure` to build a static, position-independent
  `libcfitsio.a`; `src/Makevars` links it plus OpenMP and zlib. `cleanup` removes the
  cfitsio build artefacts. Do not hand-edit `src/Makevars` paths without understanding this.
- The build takes a few minutes the first time (cfitsio compiles from source). `*.o`, `*.so`,
  `.libs/`, `.deps/` are gitignored — never commit them.
- **C++ exports:** `src/RcppExports.cpp` and `R/RcppExports.R` are generated. After adding,
  removing, or changing the signature of an `// [[Rcpp::export]]` function in `src/Rfits.cpp`,
  run `Rcpp::compileAttributes()`. Never edit the generated files by hand. Exported C++
  functions are prefixed `Cfits_`.

## Checks

```r
devtools::test()                  # or testthat::test_local()
devtools::check()                 # CI runs R CMD check with --no-manual --as-cran
```

CI is GitHub Actions (`.github/workflows/main.yml`) on macOS, Windows, and Ubuntu
(release + oldrel-1), erroring on any `R CMD check` error.

## Code style — important, this package is not tidyverse-styled

Match the existing code exactly. Do **not** reformat files, do not run styler, and do not
"modernise" surrounding code that you are not asked to change.

- **Assignment is `=`, not `<-`** (the rare `<-` occurrences are legacy; use `=`).
- **Single quotes** for strings: `'temp.fits'`, not `"temp.fits"`.
- **Function definitions** with no space before the brace: `Rfits_point = function(filename='temp.fits', ext=1){`.
- **Two-space indentation**, spaces never tabs.
- **Opening braces stay on the same line**; `if(cond){...}` without spaces around parens.
- Long argument lists wrap with two-space continuation indent under the opening paren.
- Note that indentation is **inconsistent across files** — some older files
  (`R/Rfits_header.R`, parts of `R/Rfits_methods_bespoke.R`) have function bodies at
  column 0. Match the local style of the file and block you are editing rather than
  normalising it.

### Exported function conventions

- Validate arguments at the top of the function with `checkmate` asserts, e.g.
  `assertCharacter(filename, max.len=1)`, `assertFlag(header)`, `assertIntegerish(ext, len=1)`,
  `assertAccess(filename, access='r')`.
- Immediately `filename = path.expand(filename)`, then `filename = Rfits_gunzip(filename)`
  for readers (transparently handles `.gz`; `Rfits_gunzip_clear()` runs in `.onLoad`).
- Accept `ext` as either an integer HDU index or an EXTNAME string:
  `if(is.character(ext)){ext = Rfits_extname_to_ext(filename, ext)}`.
- Constructors that build objects usually `return(invisible(output))`.
- Suggested packages are **always** guarded: `if(requireNamespace("Rwcs", quietly=TRUE))`.
  This covers `Rwcs`, `zarr`, `hdf5r`, `tdigest`, `R.utils`, `magicaxis`, `data.table`,
  `ProFound`. Never add a hard dependency for these.

### Documentation

There is **no roxygen2** in this package. `NAMESPACE` and the `man/*.Rd` files are
hand-written and must be updated manually:

- New exported function → add `export(...)` to `NAMESPACE`.
- New S3 method → add `S3method(generic, class)` to `NAMESPACE`.
- New/changed user-facing behaviour → update the relevant `man/*.Rd` (`\usage`, `\arguments`,
  `\details`, `\examples`). Topics are grouped by area (`Rfits_image.Rd`, `Rfits_table.Rd`,
  `Rfits_header.Rd`, `Rfits_methods.Rd`, `Rfits_image_zarr.Rd`, ...) — add to the existing
  grouped page rather than creating a new one, using `\alias{}` for each function name.
- `R CMD check --as-cran` will flag undocumented arguments, so keep `\usage` in sync with
  the actual signature.

## Package layout

| Path | Contents |
| --- | --- |
| `R/Rfits_all.R` | `Rfits_read_all`/`Rfits_write_all` (+ aliases `Rfits_read`/`Rfits_write`), `Rfits_make_list` |
| `R/Rfits_image.R` | Core FITS image/vector/cube/array readers, writers, `Rfits_crop`, `Rfits_tdigest`, `Rfits_blank_image` |
| `R/Rfits_table.R` | Binary/ASCII table read and write |
| `R/Rfits_header.R` | Header read/write, key manipulation, checksums, and the `Rfits_*_to_*` format converters |
| `R/Rfits_point.R` | `Rfits_point` — lazy on-disk pointer to a FITS HDU |
| `R/Rfits_image_hdf5.R`, `R/Rfits_table_hdf5.R`, `R/Rfits_all_hdf5.R` | HDF5 back-end |
| `R/Rfits_image_zarr.R` | Zarr back-end, including `Rfits_point_zarr` and S3/R2 store support |
| `R/Rfits_methods_base.R` | `print`, `plot`, `length`, `dim` methods |
| `R/Rfits_methods_subset.R` | `[` and `[<-` subsetting methods (FITS and pointers) |
| `R/Rfits_methods_operations.R` | Arithmetic/logical operator methods (`+`, `-`, `==`, `&`, ...) |
| `R/Rfits_methods_bespoke.R` | WCS-aware methods: `centre`/`center`, `corners`, `extremes`, `pixscale`, `pixarea`, `rotation` |
| `R/Rfits_methods_file.R` | `Rfits_create_RAMdisk`, `Rfits_remove_RAMdisk` |
| `R/Rfits_gunzip.R` | Transparent `.gz` handling |
| `R/utility.R` | Internal helpers (see below) |
| `src/Rfits.cpp` | All C++ (`Cfits_*`) wrappers over cfitsio |
| `inst/extdata/` | Test data: `image.fits`, `cube.fits`, `vector.fits`, `table.fits`, `profound.tab` |
| `tests/testthat/` | `test_Rfits.R` (core), `test_Rfits_zarr.R` (Zarr back-end) |
| `vignettes/Rfits_methods.Rmd` | "Methods in the Madness" — the S3 method vignette |

## Object model

Objects are **plain lists with an S3 class**, so `$` access is the normal interface.
Classes are `c('Rfits_image', 'list')` style (also `Rfits_vector`, `Rfits_cube`,
`Rfits_array`, chosen by dimensionality 1–4).

- **`Rfits_header`**: `keyvalues` (named list), `keycomments`, `keynames`, `header`
  (fixed-width 80-char card text), `hdr`, `raw` (raw bytes), `comment`, `history`, `nkey`.
- **`Rfits_image` and friends**: all the header elements plus `imDat` (the actual
  array/matrix/vector of pixel data), `filename`, `ext`, `extname`, `WCSref`.
- **`Rfits_table`**: a `data.table`/`data.frame` subclass with the header elements attached.
- **`Rfits_pointer`** (FITS), `Rfits_pointer_hdf5`, `Rfits_pointer_zarr`: lazy handles.
  They store `filename`, `ext`, `keyvalues`, `raw`, `dim`, `type` ('vector'/'image'/'cube'/
  'array') and **no data**; pixel values are only read on `[`. `Rfits_read_all` uses pointers
  automatically when the file exceeds 100 MB (`pointer='auto'`).
- **`Rfits_list`**: a list of the above with a `filename` attribute, returned by
  `Rfits_read_all` and `Rfits_make_list`.

Tile-compressed images (`ZIMAGE = TRUE`) store their real shape in `ZNAXIS*`/`ZBITPIX`
rather than `NAXIS*`/`BITPIX`. Any code reading dimensions must handle both, as
`Rfits_point` does.

## Helpers in `R/utility.R` — read the comments before touching them

Internal helpers are `.`-prefixed. Several carry long explanatory comments documenting
subtle bugs they exist to avoid; **preserve those comments** when editing, and write
similar "why" comments for new non-obvious logic.

- `.wcs2_axes()` trims a header to two celestial axes before handing it to `Rwcs`/wcslib.
  Passing a cube or 4D header through otherwise makes `ncoord`/`nelem` inconsistent and
  **corrupts memory inside `Cwcs_head_p2s`** on repeated calls. Do not remove this guard.
- `.is_whole_number()` uses `x == round(x)` with an `is.finite()` guard rather than
  `x %% 1 == 0`, because R's long-double modulo misclassifies tiny negative numbers and
  breaks on `Inf`/`NA`.
- `.is_a_to_end()` / `.resolve_a_to_end()` support the package-specific `p[10:end]` subset
  syntax. `end` means "the end of that dimension" (not `stats::end`), so the expression must
  be inspected **unevaluated** — never force it, or `50:end` fails. Bounds only are
  computed, never a full index vector (which for a pointer would be huge).
- `.safedim()` clamps overlapping source/target index ranges; `.minmax()`, `.spans_up_to()`.

## Testing conventions

`tests/testthat/test_Rfits.R` and `test_Rfits_zarr.R` use an **older, project-specific style**:

- Tests are **not** wrapped in `test_that()`. They are flat sequences of `expect_*()` calls
  with a numbered comment above each block: `#ex 1 check that we read in images like readFITS`.
  Follow this style when extending these files.
- Each block tends to be a read → write → read round trip with `expect_identical()` /
  `expect_equal()` on `imDat`, `keyvalues`, `comments`.
- Use the shipped fixtures via `system.file('extdata', 'image.fits', package = "Rfits")`.
- Write to `tempfile()`, never into the source tree or the working directory.
- Zarr tests begin with `skip_if_not_installed("zarr")` and create a unique
  `tempdir()` subdirectory per run so repeat runs cannot collide. Do the same for `hdf5r`.

## Git conventions

- Commit messages are short, lowercase, imperative-ish one-liners describing the fix
  ("Fixed some Rfits C safety issues", "array append added", "zarr pointer updates").
- Several long-lived branches exist (`master`, `zarr_stuff`, `thread_subset`, `omp_test`,
  `copilot/add-list-column-support`). **Check the current branch before committing** and do
  not switch branches or merge without being asked.
- `tests/testthat/_problems/` and `testthat-problems.rds` are local artefacts; leave them
  untracked and do not commit them.

## Practical gotchas

- A change to `src/Rfits.cpp` requires a full recompile before it is visible in an R session;
  editing only the R files can be picked up with `devtools::load_all()` (which also regenerates
  Rcpp exports).
- Memory-safety matters here: recent history is dominated by fixes for stale pointers and
  corruption in cube/array and WCS paths. When touching `[.Rfits_pointer*`, the C++ read
  paths, or anything calling `Rwcs`, verify with the existing tests and prefer defensive
  copies over in-place mutation of object elements.
- Pointer objects must keep the header information captured at creation time; do not silently
  re-open the store or file inside methods (see `.zarr_wcs_delegate()`).
- Objects returned by constructors are often `invisible()`, so tests and examples must
  assign them explicitly.
- `data.table` is used for tables; subsetting with `..cols` syntax appears in examples.
