# List the Scales an Instrument Offers

Returns the scales available for building a module, with the name to
print on a form, the camelCase stem that names the scored output column,
how many items the scale contributes, and the scale's brief
clinician-facing definition. Either name column may be passed to
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

A tibble with one row per scale and four columns: `Scale` (the display
name), `camelCase` (the scored-output stem), `nItems`, and `Brief` (the
clinician-facing definition, as
[hitopsr_definitions](https://jmgirard.github.io/hitop/reference/hitopsr_definitions.md)
carries it).

## Details

A scale whose definition is missing from the instrument's definitions
table is an error, never a blank cell: the abort carries the condition
class `hitop_missing_definition`, which a caller may catch. It is not
reachable from the shipped tables, which are built under a check that
the two tables carry the same stems.

## See also

[`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md),
which takes these names;
[hitopsr_definitions](https://jmgirard.github.io/hitop/reference/hitopsr_definitions.md),
which carries the definitions in full.

## Examples

``` r
# Every HiTOP-SR scale, with its item count
available_scales("hitopsr")
#> # A tibble: 76 × 4
#>    Scale                camelCase           nItems Brief                        
#>    <chr>                <chr>                <dbl> <chr>                        
#>  1 Agoraphobia          agoraphobia              5 Fear and avoidance of situat…
#>  2 Antisocial Behavior  antisocialBehavior       8 Behavior that goes against s…
#>  3 Appearance Focus     appearanceFocus          5 Excessive focus on appearanc…
#>  4 Appetite Loss        appetiteLoss             3 Decreased appetite and food …
#>  5 Binge Eating         bingeEating              3 Eating unusually large amoun…
#>  6 Bodily Distress      bodilyDistress           6 Physical symptoms like pain,…
#>  7 Body Dissatisfaction bodyDissatisfaction      4 Persistent dissatisfaction w…
#>  8 Callousness          callousness              6 Disregard for others' feelin…
#>  9 Checking             checking                 5 Repetitive urges to check ta…
#> 10 Cleaning             cleaning                 6 Fear of dirt and germs, lead…
#> # ℹ 66 more rows

# Pick a few and build a module from them
hitop_module("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
#> <hitop_module> hitopsr: 8 items from 2 scales
#> * Agoraphobia
#> * Appetite Loss
```
