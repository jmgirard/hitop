# List the Scales an Instrument Offers

Returns the scales available for building a module, with the name to
print on a form, the camelCase stem that names the scored output column,
and how many items the scale contributes. Either name column may be
passed to
[`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md).

This is a convenience view of the instrument's own scale table, so a
researcher choosing scales need not know which dataset to open.

## Usage

``` r
available_scales(instrument = "hitopsr")
```

## Arguments

- instrument:

  A string naming the instrument. Currently only `"hitopsr"` is
  supported. (default = `"hitopsr"`)

## Value

A tibble with one row per scale and three columns: `Scale` (the display
name), `camelCase` (the scored-output stem), and `nItems`.

## See also

[`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md),
which takes these names.

## Examples

``` r
# Every HiTOP-SR scale, with its item count
available_scales("hitopsr")
#> # A tibble: 76 × 3
#>    Scale                camelCase           nItems
#>    <chr>                <chr>                <dbl>
#>  1 Agoraphobia          agoraphobia              5
#>  2 Antisocial Behavior  antisocialBehavior       8
#>  3 Appetite Loss        appetiteLoss             3
#>  4 Binge Eating         bingeEating              3
#>  5 Bodily Distress      bodilyDistress           6
#>  6 Body Dissatisfaction bodyDissatisfaction      4
#>  7 Body Focus           bodyFocus                5
#>  8 Callousness          callousness              6
#>  9 Checking             checking                 5
#> 10 Cleaning             cleaning                 6
#> # ℹ 66 more rows

# Pick a few and build a module from them
hitop_module("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
#> <hitop_module> hitopsr: 8 items from 2 scales
#> * Agoraphobia
#> * Appetite Loss
```
