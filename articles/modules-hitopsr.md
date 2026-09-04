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
that will name its scored output column, the number of items it
contributes, and a brief clinician-facing definition (`Brief`, printed
here truncated; `hitopsr_definitions` carries it in full).

``` r

scale_menu <- available_scales("hitopsr")
scale_menu
#> # A tibble: 76 × 4
#>    Scale                camelCase           nItems Brief                        
#>    <chr>                <chr>                <int> <chr>                        
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
#> # A tibble: 5 × 4
#>   Scale                        camelCase                  nItems Brief          
#>   <chr>                        <chr>                       <int> <chr>          
#> 1 Appetite Loss                appetiteLoss                    3 "Decreased app…
#> 2 Binge Eating                 bingeEating                     3 "Eating unusua…
#> 3 Difficulties Reaching Orgasm difficultiesReachingOrgasm      3 "Trouble reach…
#> 4 Excoriation                  excoriation                     3 "Repetitive sk…
#> 5 Low Sexual Arousal           lowSexualArousal                3 "Problems with…
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

Note that `$items` holds the **original** HiTOP-SR item numbers. That is
true of the descriptor whatever a generator later prints: item 42 of the
full instrument is item 42 here, which is what lets data collected with
a module be scored against the full instrument’s key.

## Generating the Instrument

Each of the three HiTOP-SR generators takes a `module` argument. Pass
the description and you get a form containing only that module’s items.

The three do not number those items alike, on purpose. The Word form is
what a participant fills in on paper, so
[`generate_docx_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_docx_hitopsr.md)
numbers it `1` to `n` down the page – a four-scale module reads 1, 2, 3
rather than 7, 42, 213. Pass `renumber = FALSE` to keep the original
numbers instead. In the Qualtrics and REDCap exports an item number
names a collected data column, so those keep the original HiTOP-SR
numbers always; renumbering them would rename variables in dictionaries
already in the field.

A Word form also says on its face that it is a module.
[`generate_docx_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_docx_hitopsr.md)
heads a module form `HiTOP-SR Module (v1.0)` where it heads a
full-instrument form `HiTOP-SR (v1.0)`. Pass `title =` to print
something else — a study name, say — and that string is used verbatim on
either kind of form.

One combination the Word generator refuses is `include_subscales = TRUE`
together with `module`: a subscale can draw items from scales outside
the module, so its scoring row would list items the form does not
contain. The call aborts rather than print a row that cannot be scored.

[`generate_docx_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_docx_hitopsr.md)
also takes `randomize = TRUE`, which prints the items in a random order.
A renumbered module form — the default — is still numbered `1` to `n`
and carries a crosswalk from each printed number back to its original
HiTOP-SR number, printed whether or not `include_scoring` appends the
key, so it can be scored from the paper alone. That is the only case the
crosswalk is printed in. Under `renumber = FALSE` there is none, because
the printed numbers already are the original ones; and shuffling the
full instrument prints none either, since 405 pairs would fill a page.
In both of those cases read the order from the `item_order` attribute of
the returned path, or save it with `descriptor =` below. Call
[`set.seed()`](https://rdrr.io/r/base/Random.html) beforehand to make an
order reproducible.

One thing to watch:
[`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
addresses a module’s items by their position in `module$items`, which is
ascending **original** order, not the order a shuffled form prints them
in. Reorder the collected columns through `item_order` first —
`collected[order(attr(out, "item_order"))]` — or the scale scores come
back wrong with no error raised.

That recipe assumes `collected` is in the order the form printed: its
first column holds the answer to the paper’s item 1, its second the
answer to item 2, and so on, which is what data entered straight off a
shuffled form looks like. Columns already in instrument order need no
reordering, and applying the recipe to them scrambles what was right.

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
and `biosex`, and its 405 item columns are named `hsr_001` through
`hsr_405`:

``` r

data("ku_hitopsr")
names(ku_hitopsr)[1:5]
#> [1] "participant" "biosex"      "hsr_001"     "hsr_002"     "hsr_003"
```

**Select the item columns by name, never by position.** The numbers in
`four_scale$items` are item numbers, not column numbers. They coincide
only in a data frame that is exactly the 405 items in order — and this
one is not, because two other columns come first. Build the column names
instead:

``` r

item_cols <- sprintf("hsr_%03d", four_scale$items)
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
#> [1] "hsr_040" "hsr_064" "hsr_066" "hsr_107"
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
#>  1             1                     1.5              1   
#>  2             1.4                   1                1.67
#>  3             2                     1.12             1   
#>  4             1.2                   1                1.33
#>  5             1.6                   1                2.67
#>  6             1                     1                1.33
#>  7             1                     1                1   
#>  8             1                     1                1.33
#>  9             2                     1                1   
#> 10             1                     1                1   
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
  items = sprintf("hsr_%03d", 1:405),
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
#> # A tibble: 4 × 4
#>   Scale                camelCase           nItems alpha
#>   <chr>                <chr>                <int> <dbl>
#> 1 Agoraphobia          agoraphobia              5 0.820
#> 2 Antisocial Behavior  antisocialBehavior       8 0.351
#> 3 Appetite Loss        appetiteLoss             3 0.762
#> 4 Romantic Disinterest romanticDisinterest      5 0.821
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
for those. `calc_se` is **deprecated** there and here alike — it, and
the `_se` columns it adds, will be removed in a future release, and
[`interval_hitopsr()`](https://jmgirard.github.io/hitop/reference/interval_hitopsr.md)
replaces it.

## Saving the Module Beside the Form

Everything above depends on still having `four_scale` when the data
comes back. Months later, in a fresh session, rebuilding it means
retyping every scale name — and a typo scores the wrong scales while a
forgotten scale scores none.

[`write_module()`](https://jmgirard.github.io/hitop/reference/write_module.md)
saves the description to a small JSON file you can keep beside the forms
you generated:

``` r

descriptor <- write_module(four_scale, file.path(outdir, "hitopsr_module.json"))
cat(readLines(descriptor), sep = "\n")
#> {
#>   "format": "1.0",
#>   "package": "hitop",
#>   "packageVersion": "0.2.0",
#>   "buildDate": "2026-09-04",
#>   "instrument": "hitopsr",
#>   "scales": ["Agoraphobia", "Antisocial Behavior", "Appetite Loss", "Romantic Disinterest"],
#>   "items": [42, 66, 68, 109, 118, 144, 152, 156, 167, 185, 187, 202, 239, 260, 268, 274, 291, 310, 338, 389, 390],
#>   "nItems": 21
#> }
```

You do not have to remember to call it. Each of the three generators
takes a `descriptor` argument, so one call writes both the form and the
file that scores it:

``` r

generate_docx_hitopsr(
  file = file.path(outdir, "hitopsr_module2.docx"),
  module = four_scale,
  descriptor = file.path(outdir, "hitopsr_module2.json")
)
```

A call that passes no `module` writes a descriptor naming every scale,
so a full administration is described too. And on a shuffled Word form
the descriptor also records the printed order, which is the one thing a
whole-instrument form gives you nowhere else — no crosswalk is printed
for one:

``` r

set.seed(42)
shuffled_descriptor <- file.path(outdir, "hitopsr_shuffled.json")
generate_docx_hitopsr(
  file = file.path(outdir, "hitopsr_shuffled.docx"),
  module = four_scale,
  randomize = TRUE,
  descriptor = shuffled_descriptor
)

printed_order <- attr(read_module(shuffled_descriptor), "item_order")
printed_order
#>  [1] 291 118  42 185 109  66 310 338 156 152 390 167 389 144 187 274 239  68 202
#> [20] 260 268
```

Those are the original HiTOP-SR item numbers in the order the page
printed them, so responses entered off that form — columns in the order
the form printed, the first column the paper’s item 1 — go back into
instrument order with `collected[order(printed_order)]`.

The file is plain text, so you can read it, edit it, and send it to a
collaborator.
[`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md)
turns it back into a module:

``` r

reloaded <- read_module(descriptor)
reloaded
#> <hitop_module> hitopsr: 21 items from 4 scales
#> * Agoraphobia
#> * Antisocial Behavior
#> * Appetite Loss
#> * Romantic Disinterest
```

What comes back is not simply what the file said. The file records scale
*names*; the items and their reverse-keying flags are rebuilt from this
package’s own tables, so a descriptor can never introduce a scoring key
of its own. The `items` the file records are checked against that
rebuild, and a disagreement stops with an error rather than scoring
quietly — which is what you want if the file was written by an older
version of the package whose tables have since changed.

Hand the reloaded module to
[`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
exactly as you would the original:

``` r

identical(
  score_hitopsr(collected, items = names(collected), module = reloaded, append = FALSE),
  module_scores
)
#> [1] TRUE
```
