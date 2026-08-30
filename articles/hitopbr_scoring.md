# Scoring the HiTOP-BR

The HiTOP-BR instrument has 45 items and yields 8 scale scores. To
demonstrate the ability of the package to calculate these scale scores,
we can use real example data (n=411) that was collected at the
University of Kansas (KU) by Girard & Gray in 2024–2025. This data is
stored in the package under the name `ku_hitopbr`.

First, we load the package into memory using the
[`library()`](https://rdrr.io/r/base/library.html) function. If this
doesn’t work, make sure you installed the package properly (see the
README on [GitHub](https://github.com/jmgirard/hitop)).

``` r

library(hitop)
```

Next, we can load the example dataset from the package using the
[`data()`](https://rdrr.io/r/utils/data.html) function. It is a large
tibble that contains a `participant` column with a unique identifier for
each participant, a `biosex` column indicating whether each participant
is “female” or “male”, and then 45 columns numbered `hbr01` to `hbr45`
containing each participant’s rating on each item of the HiTOP-BR (on a
numerical scale from 1 to 4).

``` r

data("ku_hitopbr")
ku_hitopbr
#> # A tibble: 411 × 47
#>    participant biosex hbr01 hbr02 hbr03 hbr04 hbr05 hbr06 hbr07 hbr08 hbr09
#>    <chr>       <fct>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 P001        male       1     1     1     1     2     1     1     1     1
#>  2 P002        male       1     1     1     1     2     2     2     1     2
#>  3 P003        male       1     2     1     2     3     4     3     3     3
#>  4 P004        male       1     1     1     1     2     1     1     1     1
#>  5 P005        male       1     4     1     1     3     1     1     1     2
#>  6 P006        female     1     1     1     1     1     1     1     1     1
#>  7 P007        female     1     1     1     1     1     1     1     1     1
#>  8 P008        male       2     1     1     1     3     1     3     2     2
#>  9 P009        female     1     1     1     1     3     1     1     1     1
#> 10 P010        female     1     1     1     1     2     1     1     1     1
#> # ℹ 401 more rows
#> # ℹ 36 more variables: hbr10 <dbl>, hbr11 <dbl>, hbr12 <dbl>, hbr13 <dbl>,
#> #   hbr14 <dbl>, hbr15 <dbl>, hbr16 <dbl>, hbr17 <dbl>, hbr18 <dbl>,
#> #   hbr19 <dbl>, hbr20 <dbl>, hbr21 <dbl>, hbr22 <dbl>, hbr23 <dbl>,
#> #   hbr24 <dbl>, hbr25 <dbl>, hbr26 <dbl>, hbr27 <dbl>, hbr28 <dbl>,
#> #   hbr29 <dbl>, hbr30 <dbl>, hbr31 <dbl>, hbr32 <dbl>, hbr33 <dbl>,
#> #   hbr34 <dbl>, hbr35 <dbl>, hbr36 <dbl>, hbr37 <dbl>, hbr38 <dbl>, …
```

## Basic Scoring

To turn these item-level ratings into mean scores on the 8 scales, we
can use the
[`score_hitopbr()`](https://jmgirard.github.io/hitop/reference/score_hitopbr.md)
function. It needs to know what object contains the data and which
columns contain the item-level data. There are several ways we can
specify the items. First, we can provide the column numbers and use the
`:` shortcut. In this tibble, the items are from column 3 to column 47
so we can use `items = 3:47`. I am going to also set `append = FALSE` so
that you can quickly see the scale scores.

``` r

scores <- score_hitopbr(
  data = ku_hitopbr,
  items = 3:47,
  append = FALSE
)
scores
#> # A tibble: 411 × 8
#>    hbr_antagonism hbr_detachment hbr_disinhibition hbr_internalizing
#>             <dbl>          <dbl>             <dbl>             <dbl>
#>  1           1.44            1.4              1.33              1.12
#>  2           1.33            1.4              1.33              2.25
#>  3           2.11            2.4              2.33              2.75
#>  4           1.11            1.2              1.33              1.12
#>  5           2.44            1                2.22              1.88
#>  6           1               1.2              1.22              1.12
#>  7           1               1                1                 1   
#>  8           1.67            1.6              1.33              1.75
#>  9           1.44            1.4              1.56              1.12
#> 10           1.33            1                1                 1.25
#> # ℹ 401 more rows
#> # ℹ 4 more variables: hbr_somatoform <dbl>, hbr_thoughtDisorder <dbl>,
#> #   hbr_externalizing <dbl>, hbr_pFactor <dbl>
```

## Appending

If I had instead set `append = TRUE` (or left it off, as that is the
default), we would get back the `ku_hitopbr` tibble with the scale
scores added to the end as extra columns. Notice below how we now have
55 columns instead of 47.

``` r

scores <- score_hitopbr(
  data = ku_hitopbr,
  items = 3:47
)
scores
#> # A tibble: 411 × 55
#>    participant biosex hbr01 hbr02 hbr03 hbr04 hbr05 hbr06 hbr07 hbr08 hbr09
#>    <chr>       <fct>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 P001        male       1     1     1     1     2     1     1     1     1
#>  2 P002        male       1     1     1     1     2     2     2     1     2
#>  3 P003        male       1     2     1     2     3     4     3     3     3
#>  4 P004        male       1     1     1     1     2     1     1     1     1
#>  5 P005        male       1     4     1     1     3     1     1     1     2
#>  6 P006        female     1     1     1     1     1     1     1     1     1
#>  7 P007        female     1     1     1     1     1     1     1     1     1
#>  8 P008        male       2     1     1     1     3     1     3     2     2
#>  9 P009        female     1     1     1     1     3     1     1     1     1
#> 10 P010        female     1     1     1     1     2     1     1     1     1
#> # ℹ 401 more rows
#> # ℹ 44 more variables: hbr10 <dbl>, hbr11 <dbl>, hbr12 <dbl>, hbr13 <dbl>,
#> #   hbr14 <dbl>, hbr15 <dbl>, hbr16 <dbl>, hbr17 <dbl>, hbr18 <dbl>,
#> #   hbr19 <dbl>, hbr20 <dbl>, hbr21 <dbl>, hbr22 <dbl>, hbr23 <dbl>,
#> #   hbr24 <dbl>, hbr25 <dbl>, hbr26 <dbl>, hbr27 <dbl>, hbr28 <dbl>,
#> #   hbr29 <dbl>, hbr30 <dbl>, hbr31 <dbl>, hbr32 <dbl>, hbr33 <dbl>,
#> #   hbr34 <dbl>, hbr35 <dbl>, hbr36 <dbl>, hbr37 <dbl>, hbr38 <dbl>, …
```

## Items as Strings

Alternatively, we could provide the item column names as a character
string. Typing out all 45 item names would be a hassle, but luckily this
dataset named them consistently so we can build the names automatically
using [`sprintf()`](https://rdrr.io/r/base/sprintf.html). If we use the
“hbr%02d” format and apply that across the numbers 1 to 45, that will
create the zero-padded column names we need. If there was no
zero-padding, we could have just used “hbr%d”.

``` r

scores <- score_hitopbr(
  data = ku_hitopbr,
  items = sprintf("hbr%02d", 1:45),
  append = FALSE
)
scores
#> # A tibble: 411 × 8
#>    hbr_antagonism hbr_detachment hbr_disinhibition hbr_internalizing
#>             <dbl>          <dbl>             <dbl>             <dbl>
#>  1           1.44            1.4              1.33              1.12
#>  2           1.33            1.4              1.33              2.25
#>  3           2.11            2.4              2.33              2.75
#>  4           1.11            1.2              1.33              1.12
#>  5           2.44            1                2.22              1.88
#>  6           1               1.2              1.22              1.12
#>  7           1               1                1                 1   
#>  8           1.67            1.6              1.33              1.75
#>  9           1.44            1.4              1.56              1.12
#> 10           1.33            1                1                 1.25
#> # ℹ 401 more rows
#> # ℹ 4 more variables: hbr_somatoform <dbl>, hbr_thoughtDisorder <dbl>,
#> #   hbr_externalizing <dbl>, hbr_pFactor <dbl>
```

## Scale Prefixes

Also note that each scale column has the prefix “hbr\_” in its name. You
can change the prefix (e.g., setting it to `"hitopbr_"`) or even turn it
off (e.g., setting it to `""`) using the `prefix` argument.

``` r

scores <- score_hitopbr(
  data = ku_hitopbr,
  items = sprintf("hbr%02d", 1:45),
  prefix = "",
  append = FALSE
)
scores
#> # A tibble: 411 × 8
#>    antagonism detachment disinhibition internalizing somatoform thoughtDisorder
#>         <dbl>      <dbl>         <dbl>         <dbl>      <dbl>           <dbl>
#>  1       1.44        1.4          1.33          1.12       1.25            1   
#>  2       1.33        1.4          1.33          2.25       1.25            1   
#>  3       2.11        2.4          2.33          2.75       2.88            1.83
#>  4       1.11        1.2          1.33          1.12       1.38            1   
#>  5       2.44        1            2.22          1.88       1.25            1   
#>  6       1           1.2          1.22          1.12       1               1   
#>  7       1           1            1             1          1               1   
#>  8       1.67        1.6          1.33          1.75       1.75            1.17
#>  9       1.44        1.4          1.56          1.12       1.38            1   
#> 10       1.33        1            1             1.25       1               1   
#> # ℹ 401 more rows
#> # ℹ 2 more variables: externalizing <dbl>, pFactor <dbl>
```

## Simple Standard Errors (deprecated)

The `calc_se` argument is **deprecated**. It, and the `_se` columns it
adds, will be removed in a future release, and a call that passes
`calc_se = TRUE` now warns. Use
[`interval_hitopbr()`](https://jmgirard.github.io/hitop/reference/interval_hitopbr.md)
instead, shown under [Confidence Intervals](#confidence-intervals)
below.

What the argument computes, while it lasts: the SD of the items the
respondent actually answered divided by the square root of how many of
those items they answered. Each one summarizes how much a respondent’s
answers varied within a scale; it is not an estimate of how precisely
the scale measures the underlying trait, so it does not give a
confidence interval for a respondent’s true score. That is what replaces
it: an interval, from the reliability of the scale rather than from one
respondent’s spread of answers.

## Confidence Intervals

A scale score is measured with error, so it is worth reporting a range
rather than a single number.
[`interval_hitopbr()`](https://jmgirard.github.io/hitop/reference/interval_hitopbr.md)
returns three columns per scale: `_est`, an estimate of the respondent’s
true score, and `_lo` and `_hi`, the bounds of a confidence interval
around it.

``` r

scored <- score_hitopbr(
  data = ku_hitopbr,
  items = sprintf("hbr%02d", 1:45),
  append = FALSE
)

interval_hitopbr(
  data = scored,
  scores = c("hbr_detachment", "hbr_pFactor"),
  append = FALSE
)
#> # A tibble: 411 × 6
#>    hbr_detachment_est hbr_detachment_lo hbr_detachment_hi hbr_pFactor_est
#>                 <dbl>             <dbl>             <dbl>           <dbl>
#>  1               1.45             0.808              2.10            1.44
#>  2               1.45             0.808              2.10            1.44
#>  3               2.38             1.74               3.03            2.36
#>  4               1.27             0.622              1.91            1.13
#>  5               1.08             0.437              1.73            1.51
#>  6               1.27             0.622              1.91            1.28
#>  7               1.08             0.437              1.73            1.05
#>  8               1.64             0.993              2.28            1.51
#>  9               1.45             0.808              2.10            1.28
#> 10               1.08             0.437              1.73            1.13
#> # ℹ 401 more rows
#> # ℹ 2 more variables: hbr_pFactor_lo <dbl>, hbr_pFactor_hi <dbl>
```

The estimate is not the observed score. It is the observed score pulled
toward the reference group’s mean, because with imperfect measurement a
true score tends to lie nearer the mean than the observed score does –
the less reliable the scale, the further it is pulled. The method is the
regression approach with scale correction from Schmukle (2026), which
puts the estimate back on the same metric as the observed score so the
two can be read against each other.

The width comes from the scale’s reliability and the reference group’s
standard deviation, so it is the same for every respondent on a given
scale and it narrows as reliability rises. Widen or narrow the interval
with `level`:

``` r

interval_hitopbr(
  data = scored,
  scores = "hbr_detachment",
  level = 0.80,
  append = FALSE
)
#> # A tibble: 411 × 3
#>    hbr_detachment_est hbr_detachment_lo hbr_detachment_hi
#>                 <dbl>             <dbl>             <dbl>
#>  1               1.45             1.03               1.87
#>  2               1.45             1.03               1.87
#>  3               2.38             1.96               2.80
#>  4               1.27             0.846              1.69
#>  5               1.08             0.660              1.50
#>  6               1.27             0.846              1.69
#>  7               1.08             0.660              1.50
#>  8               1.64             1.22               2.06
#>  9               1.45             1.03               1.87
#> 10               1.08             0.660              1.50
#> # ℹ 401 more rows
```

### What the reference group is

The mean, standard deviation and reliability behind every number above
are shipped as `hitopbr_devstats`, transcribed from the Superspectra and
Spectra block of Table 1 of the HiTOP-SR introduction paper.

``` r

hitopbr_devstats
#> # A tibble: 8 × 8
#>   scale           camelCase type  nItems reliability reliabilityType  mean    sd
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

**That reference group is the paper’s Development Sample 2, N = 780
Prolific Academic participants stratified by sex and age to approximate
a community-representative United States population. It is a development
sample, and not a community norm.** No census weighting was applied and
the paper publishes no raw-score to T-score table. So an interval here
says where a score sits relative to the sample the instrument was
developed on; it does not say what percentile that score occupies in any
population.

Three further limits are worth knowing before you report one of these
intervals.

- The interval is symmetric and the same width for every respondent on a
  scale, which is what classical test theory implies, and it is **not**
  clipped to the 1-4 response range. Every HiTOP-BR scale is skewed
  enough for this to show: on all eight, a score at the response floor
  of 1 returns a lower bound below
  1.  
- The coverage the method demonstrates is across a population of
  respondents: about `level` of the intervals contain the true score
  when respondents are drawn from the reference distribution. It is not
  a guarantee for any one respondent.
- The eight scales overlap. Externalizing and p-Factor are drawn from
  the same items as the six spectrum scales rather than added to them,
  so a respondent contributes the same answers to several of these
  intervals; read them as eight views of one response set rather than
  eight independent measurements.

## Scale Reliability

As we compute scale scores, we can also estimate their inter-item
reliability using Cronbach’s α (alpha) or McDonald’s ω (omega total). α
is fast and widely used, but it assumes tau-equivalence (all items load
equally on a single factor); violations can make α under- or
over-estimate reliability. ω is based on a congeneric single-factor
model, allowing items to have different loadings and error variances; it
typically provides a more accurate reliability estimate for
unit-weighted sums. Both assume the scale is essentially unidimensional;
α and ω coincide when tau-equivalence holds.

We estimate reliability with the
[`reliability_hitopbr()`](https://jmgirard.github.io/hitop/reference/reliability_hitopbr.md)
function, which returns a tibble with one row per scale and columns for
the number of items and the requested coefficients. By default it
computes both `alpha` and `omega`; for the latter, we will need the
**lavaan** package installed (set `omega = FALSE` to skip it).

``` r

reliability_hitopbr(
  data = ku_hitopbr,
  items = sprintf("hbr%02d", 1:45)
)
#> # A tibble: 8 × 4
#>   Scale            nItems alpha omega
#>   <chr>             <int> <dbl> <dbl>
#> 1 Antagonism            9 0.805 0.811
#> 2 Detachment            5 0.801 0.792
#> 3 Disinhibition         9 0.807 0.810
#> 4 Internalizing         8 0.834 0.836
#> 5 Somatoform            8 0.825 0.832
#> 6 Thought Disorder      6 0.731 0.739
#> 7 Externalizing        10 0.817 0.818
#> 8 p-Factor             12 0.804 0.811
```
