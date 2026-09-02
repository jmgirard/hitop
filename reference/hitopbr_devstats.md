# HiTOP-BR Development-Sample Statistics

Descriptive statistics for each HiTOP-BR scale, as printed in the
"Superspectra and Spectra Scales" block of Table 1 of the HiTOP-SR
introduction paper. The reference group is that paper's **Development
Sample 2**, N = 780 Prolific Academic participants stratified by sex and
age to approximate a community-representative United States population.
It is a development sample, not a community norm: no weighting to a
census frame was applied and the paper publishes no raw-score to T-score
table. Read a score against these statistics as a comparison with the
sample the instrument was developed on.

## Usage

``` r
hitopbr_devstats
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 8
rows and 8 columns:

- Scale:

  The name of the scale. Matches
  [hitopbr_scales](https://jmgirard.github.io/hitop/reference/hitopbr_scales.md)\$Scale.

- camelCase:

  That name converted to camel case – the stem
  [`score_hitopbr()`](https://jmgirard.github.io/hitop/reference/score_hitopbr.md)
  appends to its `prefix` when it names a score column

- type:

  `"scale"` throughout. Table 1 prints all eight rows under one heading
  and labels none of them a superspectrum or a spectrum, so no such
  distinction is recorded here.

- nItems:

  The number of items in the scale (integer)

- reliability:

  The internal-consistency reliability coefficient printed for that
  scale

- reliabilityType:

  What that coefficient is. `"alpha"` throughout: Cronbach's alpha is
  what the paper prints. Supplied by this package, not read from the
  table.

- mean:

  The scale score's mean in the development sample

- sd:

  The scale score's standard deviation in the development sample

## Details

Every statistic is a printed cell of that table, transcribed and
verified against it; nothing here is computed from data by this package.
The `mean` and `sd` are on the HiTOP-BR's own four-option 1-4 response
coding, and scale scores are item means, so a score computed on another
coding is not comparable to them.
[`interval_hitopbr()`](https://jmgirard.github.io/hitop/reference/interval_hitopbr.md)
reads this table.

The HiTOP-BR scales were developed independently of the HiTOP-SR primary
scales, drawing on the same item pool, and are not a short form of them
(Table 1's Note), so these statistics are not comparable with
[hitopsr_devstats](https://jmgirard.github.io/hitop/reference/hitopsr_devstats.md).

## Item counts

Table 1's printed `# Items` agrees with the item count
[hitopbr_scales](https://jmgirard.github.io/hitop/reference/hitopbr_scales.md)
derives from
[hitopbr_items](https://jmgirard.github.io/hitop/reference/hitopbr_items.md)
for all eight scales. It did not always: item 36 ("I had a hard time
asserting myself to others.") was keyed to `Detachment` in this package
until it was corrected to `Internalizing`, the scale the instrument's
development workbook gives it in both its item-to-scale sheet and its
scoring syntax, and the scale the paper's own factor table loads it on.
`Detachment` therefore has 5 items and `Internalizing` 8, which is what
Table 1 prints for each.

## Examples

``` r
hitopbr_devstats
#> # A tibble: 8 × 8
#>   Scale           camelCase type  nItems reliability reliabilityType  mean    sd
#>   <chr>           <chr>     <chr>  <int>       <dbl> <chr>           <dbl> <dbl>
#> 1 Antagonism      antagoni… scale      9        0.82 alpha            1.42  0.45
#> 2 Detachment      detachme… scale      5        0.86 alpha            2.13  0.88
#> 3 Disinhibition   disinhib… scale      9        0.86 alpha            1.65  0.6 
#> 4 Externalizing   external… scale     10        0.83 alpha            1.54  0.49
#> 5 Internalizing   internal… scale      8        0.9  alpha            1.85  0.77
#> 6 p-Factor        pFactor   scale     12        0.86 alpha            1.68  0.55
#> 7 Somatoform      somatofo… scale      8        0.88 alpha            1.82  0.71
#> 8 Thought Disord… thoughtD… scale      6        0.85 alpha            1.26  0.46
```
