# Save a Module to a File

Writes a
[`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md)
descriptor to a JSON file, so that a researcher can keep it beside the
form they field and read it back at scoring time with
[`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md)
instead of retyping every scale name.

The file records the scale names, not the keying:
[`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md)
rebuilds the items and their reverse-keying flags from this package's
own tables. The recorded `items` are there for a human reader and as a
cross-check, and a file that disagrees with what the package derives is
an error rather than a silent preference for either side.

## Usage

``` r
write_module(module, file)
```

## Arguments

- module:

  A `hitop_module` object, as returned by
  [`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md).
  An `item_order` attribute, where present, is written as the file's
  `itemOrder` and must be a permutation of the module's items.

- file:

  A string giving the path to write to.

## Value

The `file` path, invisibly.

## The descriptor format

The file is JSON, with these fields:

- `format`:

  The format version, a `"major.minor"` string. This release writes
  `"1.0"`.

- `package`, `packageVersion`, `buildDate`:

  The package that wrote the file, its version, and the date it was
  written. Recorded for the reader;
  [`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md)
  ignores all three.

- `instrument`:

  The instrument the module belongs to.

- `scales`:

  The module's scales, as they are printed on the instrument.
  **Required**: these are what the module is rebuilt from.

- `items`, `nItems`:

  The original instrument item numbers the module covers, and how many
  there are. Cross-checked on read: the order they are written in
  carries no meaning —
  [`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md)
  compares them as a set — but a repeated number is an error, and the
  printed order of a shuffled form belongs in `itemOrder` instead.

- `itemOrder`:

  The printed order of a shuffled form: a permutation of `items`.
  Optional — a form printed in instrument order carries none.
  [`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md)
  returns it on the module's `item_order` attribute, the same attribute
  [`generate_docx_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_docx_hitopsr.md)
  returns, and `write_module()` writes it back from that attribute, so a
  descriptor read and written again keeps the order it recorded. The
  generators' `descriptor` argument sets the attribute for you.

`format`, `instrument`, and `scales` are required. The fields and the
version string are a public contract and change only deliberately.

## See also

[`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md)
to read the file back;
[`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md)
to build a module in the first place; the `descriptor` argument of
[`generate_docx_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_docx_hitopsr.md),
[`generate_qualtrics_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_qualtrics_hitopsr.md),
and
[`generate_redcap_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_redcap_hitopsr.md),
which writes one of these files beside the instrument it builds.

## Examples

``` r
m <- hitop_module("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))

f <- tempfile(fileext = ".json")
write_module(m, f)
cat(readLines(f), sep = "\n")
#> {
#>   "format": "1.0",
#>   "package": "hitop",
#>   "packageVersion": "0.2.0",
#>   "buildDate": "2026-09-01",
#>   "instrument": "hitopsr",
#>   "scales": ["Agoraphobia", "Appetite Loss"],
#>   "items": [66, 109, 118, 144, 202, 260, 291, 389],
#>   "nItems": 8
#> }

identical(read_module(f), m)
#> [1] TRUE

file.remove(f)
#> [1] TRUE
```
