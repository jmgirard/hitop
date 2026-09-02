# HiTOP-SR Development-Sample Statistics

Descriptive statistics for each HiTOP-SR primary scale and subscale, as
printed in Table 1 of the HiTOP-SR introduction paper. The reference
group is that paper's **Development Sample 2**, N = 780 Prolific
Academic participants stratified by sex and age to approximate a
community-representative United States population. It is a development
sample, not a community norm: no weighting to a census frame was applied
and the paper publishes no raw-score to T-score table. Read a score
against these statistics as a comparison with the sample the instrument
was developed on.

## Usage

``` r
hitopsr_devstats
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 93
rows and 8 columns:

- Scale:

  The name of the scale or subscale. Matches
  [hitopsr_scales](https://jmgirard.github.io/hitop/reference/hitopsr_scales.md)\$Scale
  on the scale rows and
  [hitopsr_subscales](https://jmgirard.github.io/hitop/reference/hitopsr_subscales.md)\$Subscale
  on the subscale rows.

- camelCase:

  That name converted to camel case – the stem
  [`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
  appends to its `prefix` when it names a score column

- type:

  Either `"scale"` (76 rows) or `"subscale"` (17 rows)

- nItems:

  The number of items in the scale or subscale (integer)

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
The `mean` and `sd` are on the HiTOP-SR's own four-option 1-4 response
coding, and scale scores are item means, so a score computed on another
coding is not comparable to them.
[`interval_hitopsr()`](https://jmgirard.github.io/hitop/reference/interval_hitopsr.md)
reads this table.

## Examples

``` r
hitopsr_devstats
#> # A tibble: 93 × 8
#>    Scale          camelCase type  nItems reliability reliabilityType  mean    sd
#>    <chr>          <chr>     <chr>  <int>       <dbl> <chr>           <dbl> <dbl>
#>  1 Agoraphobia    agorapho… scale      5        0.86 alpha            1.62  0.77
#>  2 Antisocial Be… antisoci… scale      8        0.86 alpha            1.07  0.25
#>  3 Appearance Fo… appearan… scale      5        0.81 alpha            1.72  0.68
#>  4 Appetite Loss  appetite… scale      3        0.8  alpha            1.53  0.68
#>  5 Binge Eating   bingeEat… scale      3        0.83 alpha            1.65  0.79
#>  6 Bodily Distre… bodilyDi… scale      6        0.85 alpha            1.84  0.74
#>  7 Body Dissatis… bodyDiss… scale      4        0.88 alpha            2.31  0.94
#>  8 Callousness    callousn… scale      6        0.84 alpha            1.47  0.55
#>  9 Checking       checking  scale      5        0.88 alpha            1.8   0.77
#> 10 Cleaning       cleaning  scale      6        0.82 alpha            1.53  0.58
#> # ℹ 83 more rows
```
