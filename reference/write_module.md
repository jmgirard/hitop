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

Before it writes, `write_module()` rebuilds the module with
[`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md)
from its `instrument` and `scales`. A module whose `items` or `nItems`
differ from that rebuild is refused, and nothing is written. The file
holds the rebuild's fields.
[`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md)
rebuilds the module from the file's `scales`, so it returns a
`hitop_module` with integer items. This is also true for a file written
from the deprecated `hitop_subset` class, or from a module whose items
are doubles.

## Usage

``` r
write_module(module, file)
```

## Arguments

- module:

  A `hitop_module` object, as returned by
  [`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md).
  An `item_order` attribute, where present, is written as the file's
  `itemOrder` and must be a permutation of the module's items. A
  `columns` attribute, where present, is written as the file's
  `columns`. It must be a character vector with one distinct, non-empty
  name per module item. A bad attribute is refused before the file is
  opened.

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
  there are. `write_module()` writes them in ascending order, and the
  hitop-form web page requires that order, refusing a descriptor whose
  `items` are not ascending. Cross-checked on read:
  [`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md)
  compares them as a set, so their order carries no meaning to it, but a
  repeated number is an error, and the printed order of a shuffled form
  belongs in `itemOrder` instead.

- `itemOrder`:

  The printed order of a shuffled form: a permutation of `items`.
  Optional — a form printed in instrument order carries none.
  [`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md)
  returns it on the module's `item_order` attribute, the same attribute
  [`generate_docx_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_docx_hitopsr.md)
  returns, and `write_module()` writes it back from that attribute, so a
  descriptor read and written again keeps the order it recorded. The
  generators' `descriptor` argument sets the attribute for you, and
  [`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
  and
  [`reliability_hitopsr()`](https://jmgirard.github.io/hitop/reference/reliability_hitopsr.md)
  read it under `layout = "printed"` to score columns entered in the
  form's printed order.

- `columns`:

  The names that an online export gives the module's items: one string
  per item, in ascending item-number order. Optional.
  [`generate_redcap_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_redcap_hitopsr.md)'s
  `descriptor` writes the dictionary's item field names here, and
  [`generate_qualtrics_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_qualtrics_hitopsr.md)'s
  writes the questions' `[[ID:]]` values. A Word form has no columns, so
  its descriptor has no field.
  [`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md)
  returns the field on the module's `columns` attribute, and
  `write_module()` writes it back from that attribute as a JSON array.
  [`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
  and
  [`reliability_hitopsr()`](https://jmgirard.github.io/hitop/reference/reliability_hitopsr.md)
  use the attribute as `items` when `items` is omitted.

`format`, `instrument`, and `scales` are required. A reader of format
`"1.0"` ignores a field it does not know, so release 0.2.0, the first
with
[`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md),
and later releases read a file with `columns` and ignore the field. The
fields and the version string are a public contract and change only
deliberately.

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
#>   "buildDate": "2026-09-24",
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
