# Read a Module from a File

Reads a module descriptor written by
[`write_module()`](https://jmgirard.github.io/hitop/reference/write_module.md)
(or written by hand to the same format) and returns the
[`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md)
object it describes, ready to pass to
[`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md),
[`reliability_hitopsr()`](https://jmgirard.github.io/hitop/reference/reliability_hitopsr.md),
or any of the generators.

The file never supplies keying. The module is rebuilt by passing the
file's `scales` through
[`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md),
so this package's own tables remain the only source of which items
belong to a scale. The file's recorded `items` and `nItems`, where
present, are checked against that rebuild, and a disagreement is an
error: a descriptor written against scale tables that have since moved
fails loudly rather than scoring quietly.

## Usage

``` r
read_module(file)
```

## Arguments

- file:

  A string giving the path to read from.

## Value

A `hitop_module` object with integer items. This is also true for a file
written from the deprecated `hitop_subset` class, or from a module whose
items are doubles. If the file carries an `itemOrder`, it is returned on
the object's `item_order` attribute — the same attribute
[`generate_docx_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_docx_hitopsr.md)
returns for a shuffled form. Pass the module to
[`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
or
[`reliability_hitopsr()`](https://jmgirard.github.io/hitop/reference/reliability_hitopsr.md)
with `layout = "printed"` to score columns entered in that printed
order. If the file carries `columns`, the names are returned as a
character vector on the object's `columns` attribute. A bare JSON string
reads as one name. Pass the module to
[`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
or
[`reliability_hitopsr()`](https://jmgirard.github.io/hitop/reference/reliability_hitopsr.md)
with `items` omitted to score the columns it names. A file with no
`columns`, or with `"columns": null`, gives a module with no such
attribute.

## Errors

Every failure below aborts with a condition naming the file, so a caller
may catch a particular one by class: `hitop_module_file_missing`,
`hitop_module_file_invalid_json`, `hitop_module_file_missing_field`,
`hitop_module_file_unsupported_format`,
`hitop_module_file_unknown_scales` (which carries
[`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md)'s
own refusal as its parent), `hitop_module_file_items_mismatch`,
`hitop_module_file_bad_item_order`, and `hitop_module_file_bad_columns`.

The list is exhaustive by design: a descriptor that is malformed rather
than merely wrong — a top level that is a JSON array instead of an
object, or a number field that is not a flat array of numbers — is
refused as `hitop_module_file_invalid_json` or as the mismatch condition
for the field it spoils, never as a bare R coercion error.

In `items`, `nItems`, and `itemOrder`, every value must be a JSON number
with a whole value. A JSON string, a JSON boolean, or a number such as
`12.4` is refused, even where it would convert to the right item number.
A JSON `null` inside an array is also refused. `items` and `nItems`
raise `hitop_module_file_items_mismatch`, and `itemOrder` raises
`hitop_module_file_bad_item_order`. A whole number written as `2.0` or
`3e0` is accepted. A field whose whole value is JSON `null` reads as
absent.

`columns` raises `hitop_module_file_bad_columns` when it is an object or
a number, when an element is not a non-empty JSON string, when it holds
a different number of names than the module has items, or when it
repeats a name.

## The descriptor format

The file is JSON, with these fields:

- `format`:

  The format version, a `"major.minor"` string. This release writes
  `"1.0"`.

- `package`, `packageVersion`, `buildDate`:

  The package that wrote the file, its version, and the date it was
  written. Recorded for the reader; `read_module()` ignores all three.

- `instrument`:

  The instrument the module belongs to.

- `scales`:

  The module's scales, as they are printed on the instrument.
  **Required**: these are what the module is rebuilt from.

- `items`, `nItems`:

  The original instrument item numbers the module covers, and how many
  there are. Cross-checked on read: the order they are written in
  carries no meaning — `read_module()` compares them as a set — but a
  repeated number is an error, and the printed order of a shuffled form
  belongs in `itemOrder` instead.

- `itemOrder`:

  The printed order of a shuffled form: a permutation of `items`.
  Optional — a form printed in instrument order carries none.
  `read_module()` returns it on the module's `item_order` attribute, the
  same attribute
  [`generate_docx_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_docx_hitopsr.md)
  returns, and
  [`write_module()`](https://jmgirard.github.io/hitop/reference/write_module.md)
  writes it back from that attribute, so a descriptor read and written
  again keeps the order it recorded. The generators' `descriptor`
  argument sets the attribute for you, and
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
  its descriptor has no field. `read_module()` returns the field on the
  module's `columns` attribute, and
  [`write_module()`](https://jmgirard.github.io/hitop/reference/write_module.md)
  writes it back from that attribute as a JSON array.
  [`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
  and
  [`reliability_hitopsr()`](https://jmgirard.github.io/hitop/reference/reliability_hitopsr.md)
  use the attribute as `items` when `items` is omitted.

`format`, `instrument`, and `scales` are required. A reader of format
`"1.0"` ignores a field it does not know, so release 0.2.0, the first
with `read_module()`, and later releases read a file with `columns` and
ignore the field. The fields and the version string are a public
contract and change only deliberately.

## See also

[`write_module()`](https://jmgirard.github.io/hitop/reference/write_module.md)
to write the file;
[`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md)
to build a module without one.

## Examples

``` r
m <- hitop_module("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))

f <- tempfile(fileext = ".json")
write_module(m, f)

m2 <- read_module(f)
m2
#> <hitop_module> hitopsr: 8 items from 2 scales
#> * Agoraphobia
#> * Appetite Loss
identical(m2, m)
#> [1] TRUE

file.remove(f)
#> [1] TRUE
```
