# Building HiTOP-SR Modules

The full HiTOP-SR asks 405 items and returns 76 scale scores, which is
more than many studies need and more than many participants will sit
through. A **module** is a chosen set of the instrument’s scales,
administered and scored on its own.

This article walks the whole module workflow end to end: choosing which
scales to include, describing the module, generating the files you will
actually field, and scoring the data that comes back. For scoring a
*complete* administration, see [Scoring the
HiTOP-SR](https://jmgirard.github.io/hitop/articles/hitopsr_scoring.md)
instead.

``` r

library(hitop)
```

## Choosing Scales

Start from the menu.
[`available_scales()`](https://jmgirard.github.io/hitop/reference/available_scales.md)
lists every scale the instrument offers, along with the camelCase stem
that will name its scored output column and the number of items it
contributes.

``` r

scale_menu <- available_scales("hitopsr")
scale_menu
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
```

Both name columns are accepted when you build a module, so you can write
scale names the way they are printed on the form
(`"Antisocial Behavior"`) or the way they appear in scored output
(`"antisocialBehavior"`).

A module is built from these 76 scales only. The instrument also defines
17 subscales (see `hitopsr_subscales`), but a subscale is not a unit you
can select here —
[`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md)
rejects a subscale name as unknown. Ask for the scale that contains it
instead.

Because this is an ordinary tibble, you can also use it to plan the
length of your module. For example, to see the shortest scales:

``` r

scale_menu[order(scale_menu$nItems), ][1:5, ]
#> # A tibble: 5 × 3
#>   Scale                        camelCase                  nItems
#>   <chr>                        <chr>                       <dbl>
#> 1 Appetite Loss                appetiteLoss                    3
#> 2 Binge Eating                 bingeEating                     3
#> 3 Difficulties Reaching Orgasm difficultiesReachingOrgasm      3
#> 4 Excoriation                  excoriation                     3
#> 5 Low Sexual Arousal           lowSexualArousal                3
```

## Building the Module

[`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md)
turns a set of scale names into a validated description of the module.
It resolves the names, works out which items those scales need, and
records the reverse-keying flags that scoring will require.

``` r

four_scale <- hitop_module(
  instrument = "hitopsr",
  scales = c(
    "Agoraphobia", "Appetite Loss",
    "Antisocial Behavior", "Romantic Disinterest"
  )
)
four_scale
#> <hitop_module> hitopsr: 21 items from 4 scales
#> * Agoraphobia
#> * Antisocial Behavior
#> * Appetite Loss
#> * Romantic Disinterest
```

The printed summary reports how many items the module needs. The
description itself is a plain list, so you can look inside it:

``` r

four_scale$camelCase
#> [1] "agoraphobia"         "antisocialBehavior"  "appetiteLoss"       
#> [4] "romanticDisinterest"
four_scale$nItems
#> [1] 21
four_scale$items
#>  [1]  42  66  68 109 118 144 152 156 167 185 187 202 239 260 268 274 291 310 338
#> [20] 389 390
```

Note that `$items` holds the **original** HiTOP-SR item numbers. A
module does not renumber its items from 1: item 42 of the full
instrument stays item 42 throughout, so data collected with a module can
still be scored against the full instrument’s key.

## Generating the Instrument

Each of the three HiTOP-SR generators takes a `module` argument. Pass
the description and you get a form containing only that module’s items,
keeping their original numbers.

Here we write all three formats into a temporary folder; in your own
work you would give a real path or let the default filename land in your
working directory.

``` r

outdir <- tempdir()

docx_file <- generate_docx_hitopsr(
  file = file.path(outdir, "hitopsr_module.docx"),
  module = four_scale
)

qualtrics_file <- generate_qualtrics_hitopsr(
  file = file.path(outdir, "hitopsr_module.txt"),
  module = four_scale
)

redcap_file <- generate_redcap_hitopsr(
  file = file.path(outdir, "hitopsr_module.zip"),
  module = four_scale
)
```

``` r

basename(c(docx_file, qualtrics_file, redcap_file))
#> [1] "hitopsr_module.docx" "hitopsr_module.txt"  "hitopsr_module.zip"
```

The DOCX file is a paper form ready to print, the `.txt` file imports
into Qualtrics as an advanced-format question block, and the `.zip` file
imports into REDCap as an instrument. See [Importing Instruments into
Qualtrics and
REDCap](https://jmgirard.github.io/hitop/articles/import-instructions.md)
for the import steps themselves.

## Selecting the Collected Columns

Once the data comes back, it has only the module’s item columns — plus
whatever else your platform recorded, such as participant identifiers
and demographics. Both scoring functions need you to name those item
columns.

We will stand in for module-collected data using `ku_hitopsr`, the
package’s real example dataset. Its first two columns are `participant`
and `biosex`, and its 405 item columns are named `hsr001` through
`hsr405`:

``` r

data("ku_hitopsr")
names(ku_hitopsr)[1:5]
#> [1] "participant" "biosex"      "hsr001"      "hsr002"      "hsr003"
```

**Select the item columns by name, never by position.** The numbers in
`four_scale$items` are item numbers, not column numbers. They coincide
only in a data frame that is exactly the 405 items in order — and this
one is not, because two other columns come first. Build the column names
instead:

``` r

item_cols <- sprintf("hsr%03d", four_scale$items)
collected <- ku_hitopsr[item_cols]
ncol(collected)
#> [1] 21
```

That width matches the module’s own item count, which is the check worth
running before you score anything:

``` r

ncol(collected) == four_scale$nItems
#> [1] TRUE
```

Indexing by `four_scale$items` directly would have selected columns 42,
66, 68, and so on *by position*, which in this frame are the wrong items
— shifted by the two leading columns:

``` r

names(ku_hitopsr[four_scale$items])[1:4]
#> [1] "hsr040" "hsr064" "hsr066" "hsr107"
```

Those are items 40, 64, 66, and 107 masquerading as the module’s items.
Nothing would have failed loudly; the scores would simply have been
wrong. If your item columns carry other names,
[`rename_hitopsr_items()`](https://jmgirard.github.io/hitop/reference/rename_hitopsr_items.md)
can rename them to standard item numbers first; its `prefix` argument
sets the stem those names are built from.

## Scoring and Reliability

Hand the same description back through the `module` argument. Without
it,
[`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
expects a full administration and stops on the item count (“Expected 405
items but got 21”); with it, the `items` argument names the columns you
actually have, in instrument order, and `module` tells the function
which scales they belong to.

``` r

module_scores <- score_hitopsr(
  data = collected,
  items = names(collected),
  module = four_scale,
  append = FALSE
)
module_scores
#> # A tibble: 411 × 4
#>    hsr_agoraphobia hsr_antisocialBehavior hsr_appetiteLoss
#>              <dbl>                  <dbl>            <dbl>
#>  1             2                     1.12             1   
#>  2             1.4                   1.75             1   
#>  3             2.2                   2.12             2   
#>  4             1.2                   1.25             1   
#>  5             2                     1.88             2   
#>  6             1                     1.25             1   
#>  7             1                     1                1.67
#>  8             1.6                   1.62             1   
#>  9             1.4                   1.25             1.67
#> 10             1.2                   1.38             1   
#> # ℹ 401 more rows
#> # ℹ 1 more variable: hsr_romanticDisinterest <dbl>
```

Only the module’s scales come back, in the order they appear in
`hitopsr_scales`. The values are exactly what a full administration
would have produced for those scales — a scale score depends only on its
own items, so dropping the other 72 scales’ columns cannot move it:

``` r

full_scores <- score_hitopsr(
  data = ku_hitopsr,
  items = sprintf("hsr%03d", 1:405),
  append = FALSE
)
all.equal(module_scores, full_scores[names(module_scores)])
#> [1] TRUE
```

[`reliability_hitopsr()`](https://jmgirard.github.io/hitop/reference/reliability_hitopsr.md)
takes the same argument and returns one row per module scale.

``` r

reliability_hitopsr(
  data = collected,
  items = names(collected),
  module = four_scale,
  omega = FALSE
)
#> # A tibble: 4 × 3
#>   scale                nItems   alpha
#>   <chr>                 <int>   <dbl>
#> 1 Agoraphobia               5  0.419 
#> 2 Antisocial Behavior       8  0.545 
#> 3 Appetite Loss             3  0.367 
#> 4 Romantic Disinterest      5 -0.0803
```

Reliability is worth checking on your own module rather than assumed. In
this sample the four coefficients run from modest down to negative:
Romantic Disinterest returns a negative α, which is not a small number
but a sign that its items covary near zero or oppositely in these data,
so a mean of them is not measuring one thing. Short scales make α less
stable, but shortness alone does not produce a negative value. Set
`omega = FALSE` to skip McDonald’s ω, which needs the **lavaan**
package.

The `srange`, `prefix`, `missing`, `calc_se`, and `append` arguments all
behave exactly as they do for a full administration; see [Scoring the
HiTOP-SR](https://jmgirard.github.io/hitop/articles/hitopsr_scoring.md)
for those.
