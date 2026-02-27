# Scoring the HiTOP-BR

The HiTOP-BR instrument has 45 items and yields 7 scale scores. To
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

To turn these item-level ratings into mean scores on the 7 scales, we
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
#>  1           1.44           1.33              1.33              1.14
#>  2           1.33           1.33              1.33              2.43
#>  3           2.11           2.33              2.33              2.86
#>  4           1.11           1.17              1.33              1.14
#>  5           2.44           1.17              2.22              1.86
#>  6           1              1.17              1.22              1.14
#>  7           1              1                 1                 1   
#>  8           1.67           1.5               1.33              1.86
#>  9           1.44           1.33              1.56              1.14
#> 10           1.33           1                 1                 1.29
#> # ℹ 401 more rows
#> # ℹ 4 more variables: hbr_somatoform <dbl>, hbr_thoughtDisorder <dbl>,
#> #   hbr_externalizing <dbl>, hbr_pFactor <dbl>
```

## Appending

If I had instead set `append = TRUE` (or left it off, as that is the
default), we would get back the `ku_hitopbr` tibble with the scale
scores added to the end as extra columns. Notice below how we now have
54 columns instead of 47.

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
#>  1           1.44           1.33              1.33              1.14
#>  2           1.33           1.33              1.33              2.43
#>  3           2.11           2.33              2.33              2.86
#>  4           1.11           1.17              1.33              1.14
#>  5           2.44           1.17              2.22              1.86
#>  6           1              1.17              1.22              1.14
#>  7           1              1                 1                 1   
#>  8           1.67           1.5               1.33              1.86
#>  9           1.44           1.33              1.56              1.14
#> 10           1.33           1                 1                 1.29
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
#>  1       1.44       1.33          1.33          1.14       1.25            1   
#>  2       1.33       1.33          1.33          2.43       1.25            1   
#>  3       2.11       2.33          2.33          2.86       2.88            1.83
#>  4       1.11       1.17          1.33          1.14       1.38            1   
#>  5       2.44       1.17          2.22          1.86       1.25            1   
#>  6       1          1.17          1.22          1.14       1               1   
#>  7       1          1             1             1          1               1   
#>  8       1.67       1.5           1.33          1.86       1.75            1.17
#>  9       1.44       1.33          1.56          1.14       1.38            1   
#> 10       1.33       1             1             1.29       1               1   
#> # ℹ 401 more rows
#> # ℹ 2 more variables: externalizing <dbl>, pFactor <dbl>
```

## Simple Standard Errors

Finally, in addition to calculating each scale score as the mean of its
corresponding items, we can also calculate each scale score’s standard
error as the SD of its corresponding items divided by the square root of
its number of items. These standard errors are especially useful when
plotting the scores as they can be converted into confidence intervals.
We turn this on using `calc_se`.

``` r
scores <- score_hitopbr(
  data = ku_hitopbr,
  items = sprintf("hbr%02d", 1:45),
  calc_se = TRUE,
  append = FALSE
)
scores
#> # A tibble: 411 × 16
#>    hbr_antagonism hbr_detachment hbr_disinhibition hbr_internalizing
#>             <dbl>          <dbl>             <dbl>             <dbl>
#>  1           1.44           1.33              1.33              1.14
#>  2           1.33           1.33              1.33              2.43
#>  3           2.11           2.33              2.33              2.86
#>  4           1.11           1.17              1.33              1.14
#>  5           2.44           1.17              2.22              1.86
#>  6           1              1.17              1.22              1.14
#>  7           1              1                 1                 1   
#>  8           1.67           1.5               1.33              1.86
#>  9           1.44           1.33              1.56              1.14
#> 10           1.33           1                 1                 1.29
#> # ℹ 401 more rows
#> # ℹ 12 more variables: hbr_somatoform <dbl>, hbr_thoughtDisorder <dbl>,
#> #   hbr_externalizing <dbl>, hbr_pFactor <dbl>, hbr_antagonism_se <dbl>,
#> #   hbr_detachment_se <dbl>, hbr_disinhibition_se <dbl>,
#> #   hbr_internalizing_se <dbl>, hbr_somatoform_se <dbl>,
#> #   hbr_thoughtDisorder_se <dbl>, hbr_externalizing_se <dbl>,
#> #   hbr_pFactor_se <dbl>
```

Note how there are now 14 columns instead of 7. The extra columns aren’t
shown in the preview above, but they are named with the `_se` suffix,
e.g., `hbr_somatoform_se`.
