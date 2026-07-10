# Scoring the HiTOP-SR

The HiTOP-SR instrument has 405 items and yields 76 scale scores. To
demonstrate the ability of the package to calculate these scale scores,
we can use real example data (n=411) that was collected at the
University of Kansas (KU) by Girard & Gray in 2024–2025. This data is
stored in the package under the name `ku_hitopsr`.

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
is “female” or “male”, and then 405 columns numbered `hitop001` to
`hitop405` containing each participant’s rating on each item of the
HiTOP-SR (on a numerical scale from 1 to 4).

``` r

data("ku_hitopsr")
ku_hitopsr
#> # A tibble: 411 × 407
#>    participant biosex hsr001 hsr002 hsr003 hsr004 hsr005 hsr006 hsr007 hsr008
#>    <chr>       <fct>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#>  1 P001        male        1      1      1      1      1      2      1      2
#>  2 P002        male        2      1      1      1      2      1      1      1
#>  3 P003        male        1      2      2      3      2      1      1      1
#>  4 P004        male        1      1      2      1      1      1      1      1
#>  5 P005        male        1      2      1      1      3      1      1      1
#>  6 P006        female      1      1      1      1      1      1      1      1
#>  7 P007        female      1      1      1      1      1      1      1      1
#>  8 P008        male        1      1      1      1      1      1      1      1
#>  9 P009        female      3      2      3      1      1      1      1      1
#> 10 P010        female      1      1      1      1      1      1      1      1
#> # ℹ 401 more rows
#> # ℹ 397 more variables: hsr009 <dbl>, hsr010 <dbl>, hsr011 <dbl>, hsr012 <dbl>,
#> #   hsr013 <dbl>, hsr014 <dbl>, hsr015 <dbl>, hsr016 <dbl>, hsr017 <dbl>,
#> #   hsr018 <dbl>, hsr019 <dbl>, hsr020 <dbl>, hsr021 <dbl>, hsr022 <dbl>,
#> #   hsr023 <dbl>, hsr024 <dbl>, hsr025 <dbl>, hsr026 <dbl>, hsr027 <dbl>,
#> #   hsr028 <dbl>, hsr029 <dbl>, hsr030 <dbl>, hsr031 <dbl>, hsr032 <dbl>,
#> #   hsr033 <dbl>, hsr034 <dbl>, hsr035 <dbl>, hsr036 <dbl>, hsr037 <dbl>, …
```

## Basic Scoring

To turn these item-level ratings into mean scores on the 76 scales, we
can use the
[`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
function. It needs to know what object contains the data and which
columns contain the item-level data. There are several ways we can
specify the items. First, we can provide the column numbers and use the
`:` shortcut. In this tibble, the items are from column 3 to column 407
so we can use `items = 3:407`. I am going to also set `append = FALSE`
so that you can quickly see the scale scores.

``` r

scores <- score_hitopsr(
  data = ku_hitopsr,
  items = 3:407,
  append = FALSE
)
scores
#> # A tibble: 411 × 76
#>    hsr_agoraphobia hsr_antisocialBehavior hsr_appetiteLoss hsr_bingeEating
#>              <dbl>                  <dbl>            <dbl>           <dbl>
#>  1             2                     1.12             1               1.67
#>  2             1.4                   1.75             1               2.67
#>  3             2.2                   2.12             2               2.33
#>  4             1.2                   1.25             1               2.33
#>  5             2                     1.88             2               2.33
#>  6             1                     1.25             1               1.33
#>  7             1                     1                1.67            1.67
#>  8             1.6                   1.62             1               1.33
#>  9             1.4                   1.25             1.67            2   
#> 10             1.2                   1.38             1               1   
#> # ℹ 401 more rows
#> # ℹ 72 more variables: hsr_bodilyDistress <dbl>, hsr_bodyDissatisfaction <dbl>,
#> #   hsr_bodyFocus <dbl>, hsr_callousness <dbl>, hsr_checking <dbl>,
#> #   hsr_cleaning <dbl>, hsr_cognitiveProblems <dbl>,
#> #   hsr_conversionSymptoms <dbl>, hsr_counting <dbl>,
#> #   hsr_dietaryRestraint <dbl>, hsr_difficultiesReachingOrgasm <dbl>,
#> #   hsr_diseaseConviction <dbl>, hsr_dishonesty <dbl>, …
```

## Appending

If I had instead set `append = TRUE` (or left it off, as that is the
default), we would get back the `ku_hitoppro` tibble with the scale
scores added to the end as extra columns. Notice below how we now have
483 columns instead of 407.

``` r

scores <- score_hitopsr(
  data = ku_hitopsr,
  items = 3:407
)
scores
#> # A tibble: 411 × 483
#>    participant biosex hsr001 hsr002 hsr003 hsr004 hsr005 hsr006 hsr007 hsr008
#>    <chr>       <fct>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#>  1 P001        male        1      1      1      1      1      2      1      2
#>  2 P002        male        2      1      1      1      2      1      1      1
#>  3 P003        male        1      2      2      3      2      1      1      1
#>  4 P004        male        1      1      2      1      1      1      1      1
#>  5 P005        male        1      2      1      1      3      1      1      1
#>  6 P006        female      1      1      1      1      1      1      1      1
#>  7 P007        female      1      1      1      1      1      1      1      1
#>  8 P008        male        1      1      1      1      1      1      1      1
#>  9 P009        female      3      2      3      1      1      1      1      1
#> 10 P010        female      1      1      1      1      1      1      1      1
#> # ℹ 401 more rows
#> # ℹ 473 more variables: hsr009 <dbl>, hsr010 <dbl>, hsr011 <dbl>, hsr012 <dbl>,
#> #   hsr013 <dbl>, hsr014 <dbl>, hsr015 <dbl>, hsr016 <dbl>, hsr017 <dbl>,
#> #   hsr018 <dbl>, hsr019 <dbl>, hsr020 <dbl>, hsr021 <dbl>, hsr022 <dbl>,
#> #   hsr023 <dbl>, hsr024 <dbl>, hsr025 <dbl>, hsr026 <dbl>, hsr027 <dbl>,
#> #   hsr028 <dbl>, hsr029 <dbl>, hsr030 <dbl>, hsr031 <dbl>, hsr032 <dbl>,
#> #   hsr033 <dbl>, hsr034 <dbl>, hsr035 <dbl>, hsr036 <dbl>, hsr037 <dbl>, …
```

## Items as Strings

Alternatively, we could provide the item column names as a character
string. Typing out all 405 item names would be a hassle, but luckily
this dataset named them consistently so we can build the names
automatically using [`sprintf()`](https://rdrr.io/r/base/sprintf.html).
If we use the “hitop%03d” format and apply that across the numbers 1 to
405, that will create the zero-padded column names we need. If there was
no zero-padding, we could have just used “hitop%d”.

``` r

scores <- score_hitopsr(
  data = ku_hitopsr,
  items = sprintf("hsr%03d", 1:405),
  append = FALSE
)
scores
#> # A tibble: 411 × 76
#>    hsr_agoraphobia hsr_antisocialBehavior hsr_appetiteLoss hsr_bingeEating
#>              <dbl>                  <dbl>            <dbl>           <dbl>
#>  1             2                     1.12             1               1.67
#>  2             1.4                   1.75             1               2.67
#>  3             2.2                   2.12             2               2.33
#>  4             1.2                   1.25             1               2.33
#>  5             2                     1.88             2               2.33
#>  6             1                     1.25             1               1.33
#>  7             1                     1                1.67            1.67
#>  8             1.6                   1.62             1               1.33
#>  9             1.4                   1.25             1.67            2   
#> 10             1.2                   1.38             1               1   
#> # ℹ 401 more rows
#> # ℹ 72 more variables: hsr_bodilyDistress <dbl>, hsr_bodyDissatisfaction <dbl>,
#> #   hsr_bodyFocus <dbl>, hsr_callousness <dbl>, hsr_checking <dbl>,
#> #   hsr_cleaning <dbl>, hsr_cognitiveProblems <dbl>,
#> #   hsr_conversionSymptoms <dbl>, hsr_counting <dbl>,
#> #   hsr_dietaryRestraint <dbl>, hsr_difficultiesReachingOrgasm <dbl>,
#> #   hsr_diseaseConviction <dbl>, hsr_dishonesty <dbl>, …
```

## Scale Prefixes

Also note that each scale column has the prefix “hsr\_” in its name. You
can change the prefix (e.g., setting it to `"hitop_"`) or even turn it
off (e.g., setting it to `""`) using the `prefix` argument.

``` r

scores <- score_hitopsr(
  data = ku_hitopsr,
  items = sprintf("hsr%03d", 1:405),
  prefix = "hitop_",
  append = FALSE
)
scores
#> # A tibble: 411 × 76
#>    hitop_agoraphobia hitop_antisocialBeha…¹ hitop_appetiteLoss hitop_bingeEating
#>                <dbl>                  <dbl>              <dbl>             <dbl>
#>  1               2                     1.12               1                 1.67
#>  2               1.4                   1.75               1                 2.67
#>  3               2.2                   2.12               2                 2.33
#>  4               1.2                   1.25               1                 2.33
#>  5               2                     1.88               2                 2.33
#>  6               1                     1.25               1                 1.33
#>  7               1                     1                  1.67              1.67
#>  8               1.6                   1.62               1                 1.33
#>  9               1.4                   1.25               1.67              2   
#> 10               1.2                   1.38               1                 1   
#> # ℹ 401 more rows
#> # ℹ abbreviated name: ¹​hitop_antisocialBehavior
#> # ℹ 72 more variables: hitop_bodilyDistress <dbl>,
#> #   hitop_bodyDissatisfaction <dbl>, hitop_bodyFocus <dbl>,
#> #   hitop_callousness <dbl>, hitop_checking <dbl>, hitop_cleaning <dbl>,
#> #   hitop_cognitiveProblems <dbl>, hitop_conversionSymptoms <dbl>,
#> #   hitop_counting <dbl>, hitop_dietaryRestraint <dbl>, …
```

## Simple Standard Errors

In addition to calculating each scale score as the mean of its
corresponding items, we can also calculate each scale score’s standard
error as the SD of its corresponding items divided by the square root of
its number of items. These standard errors are especially useful when
plotting the scores as they can be converted into confidence intervals.
We turn this on using `calc_se`.

``` r

scores <- score_hitopsr(
  data = ku_hitopsr,
  items = sprintf("hsr%03d", 1:405),
  calc_se = TRUE,
  append = FALSE
)
scores
#> # A tibble: 411 × 152
#>    hsr_agoraphobia hsr_antisocialBehavior hsr_appetiteLoss hsr_bingeEating
#>              <dbl>                  <dbl>            <dbl>           <dbl>
#>  1             2                     1.12             1               1.67
#>  2             1.4                   1.75             1               2.67
#>  3             2.2                   2.12             2               2.33
#>  4             1.2                   1.25             1               2.33
#>  5             2                     1.88             2               2.33
#>  6             1                     1.25             1               1.33
#>  7             1                     1                1.67            1.67
#>  8             1.6                   1.62             1               1.33
#>  9             1.4                   1.25             1.67            2   
#> 10             1.2                   1.38             1               1   
#> # ℹ 401 more rows
#> # ℹ 148 more variables: hsr_bodilyDistress <dbl>,
#> #   hsr_bodyDissatisfaction <dbl>, hsr_bodyFocus <dbl>, hsr_callousness <dbl>,
#> #   hsr_checking <dbl>, hsr_cleaning <dbl>, hsr_cognitiveProblems <dbl>,
#> #   hsr_conversionSymptoms <dbl>, hsr_counting <dbl>,
#> #   hsr_dietaryRestraint <dbl>, hsr_difficultiesReachingOrgasm <dbl>,
#> #   hsr_diseaseConviction <dbl>, hsr_dishonesty <dbl>, …
```

Note how there are now 152 columns instead of 76. The extra columns
aren’t shown in the preview above, but they are named with the `_se`
suffix, e.g., `hsr_agoraphobia_se`.

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
[`reliability_hitopsr()`](https://jmgirard.github.io/hitop/reference/reliability_hitopsr.md)
function, which returns a tibble with one row per scale and columns for
the number of items and the requested coefficients. By default it
computes both `alpha` and `omega`; for the latter, we will need the
**lavaan** package installed (set `omega = FALSE` to skip it).

``` r

reliability_hitopsr(
  data = ku_hitopsr,
  items = sprintf("hsr%03d", 1:405)
)
#> # A tibble: 76 × 4
#>    scale                nItems alpha  omega
#>    <chr>                 <int> <dbl>  <dbl>
#>  1 Agoraphobia               5 0.419  0.431
#>  2 Antisocial Behavior       8 0.545  0.553
#>  3 Appetite Loss             3 0.367 NA    
#>  4 Binge Eating              3 0.110  0.218
#>  5 Bodily Distress           6 0.396  0.469
#>  6 Body Dissatisfaction      4 0.294 NA    
#>  7 Body Focus                5 0.394  0.436
#>  8 Callousness               6 0.482  0.490
#>  9 Checking                  5 0.601  0.605
#> 10 Cleaning                  6 0.480  0.526
#> # ℹ 66 more rows
```

## Renaming Item Columns

If your dataset was collected before the item numbers were standardized
or used arbitrary variable names, you must rename your columns to the
standard format before running the scoring engine. The
[`rename_hitopsr_items()`](https://jmgirard.github.io/hitop/reference/rename_hitopsr_items.md)
function handles this preparation via two approaches.

### Method 1: Legacy “Original” Pool Names

If your columns are labeled with older pool names (e.g., HiTOP_659 or
Ext_432), use `method = "original"`. The function scans your data frame
columns, matches them against the package’s built-in item database, and
renames them to standard names.

``` r

# Setup a mock dataset with older legacy column names
legacy_data <- data.frame(
  HiTOP_659 = c(1, 2, 4),
  HiTOP_301 = c(3, 4, 2),
  ParticipantID = c(101, 102, 103)
)

# Rename legacy columns using the native pipe
standardized_legacy <-
  legacy_data |>
  rename_hitopsr_items(method = "original", prefix = "HSR_")
#> Warning: Only 2 out of 405 HiTOP-SR items were successfully matched and renamed.
#> ℹ Note: If you plan to use `score_hitopsr()`, ensure uncollected items exist in
#>   the data frame as `NA` columns.

standardized_legacy
#>   HSR_1 HSR_2 ParticipantID
#> 1     1     3           101
#> 2     2     4           102
#> 3     4     2           103
```

### Method 2: Matching via Literal Item Text

If your dataset uses completely customized column names but you tracked
the exact question prompts presented to your participants, use
`method = "text"`. Provide a vector of your current column names
alongside a matching vector of literal item text strings.

``` r

# Setup a mock dataset with completely custom column names
custom_data <- data.frame(
  q_party = c(1, 2, 1),
  q_flawless = c(4, 3, 4),
  Age = c(21, 25, 30)
)

# Define the mapping pairs
my_cols <- c("q_party", "q_flawless")
my_texts <- c(
  "I preferred to stay home than to go to a party.",
  "I felt that my work must be flawless."
)

# Rename custom columns based on text matching
standardized_custom <- custom_data |>
  rename_hitopsr_items(
    method = "text",
    item_cols = my_cols,
    item_text = my_texts,
    prefix = "HSR_"
  )
#> Warning: Only 2 out of 405 HiTOP-SR items were successfully matched and renamed.
#> ℹ Note: If you plan to use `score_hitopsr()`, ensure uncollected items exist in
#>   the data frame as `NA` columns.

standardized_custom
#>   HSR_1 HSR_2 Age
#> 1     1     4  21
#> 2     2     3  25
#> 3     1     4  30
```

*Note: Because these small mock examples contain only a subset of the
full item pool,
[`rename_hitopsr_items()`](https://jmgirard.github.io/hitop/reference/rename_hitopsr_items.md)
will safely issue a cli warning letting you know that fewer than 405
items were matched. This perfectly accommodates researchers
intentionally administering short forms or separate diagnostic modules.*
