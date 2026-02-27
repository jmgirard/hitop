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

We can just add one or more of the following arguments to
[`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md):
`alpha` and `omega`. For the latter, we will need the **lavaan** package
installed. If requested, a table of reliability results will be printed
as a side-effect of the function (alongside any warnings from lavaan
about convergence of the factor analysis models that omega is based on).

``` r
scores <- score_hitopsr(
  data = ku_hitopsr,
  items = sprintf("hsr%03d", 1:405),
  alpha = TRUE,
  omega = TRUE
)
#>                           scale   alpha omega
#> 1                   Agoraphobia  0.4188 0.431
#> 2           Antisocial Behavior  0.5446 0.553
#> 3                 Appetite Loss  0.3666    NA
#> 4                  Binge Eating  0.1103 0.218
#> 5               Bodily Distress  0.3960 0.469
#> 6          Body Dissatisfaction  0.2935    NA
#> 7                    Body Focus  0.3939 0.436
#> 8                   Callousness  0.4816 0.490
#> 9                      Checking  0.6014 0.605
#> 10                     Cleaning  0.4800 0.526
#> 11           Cognitive Problems  0.4744 0.508
#> 12          Conversion Symptoms  0.4546 0.487
#> 13                     Counting  0.2714 0.333
#> 14            Dietary Restraint  0.5214 0.543
#> 15 Difficulties Reaching Orgasm  0.4128 0.447
#> 16           Disease Conviction  0.5616 0.574
#> 17                   Dishonesty  0.5520 0.579
#> 18              Disorganization  0.4663 0.491
#> 19                 Dissociation  0.4384 0.456
#> 20           Distress Dysphoria  0.7408 0.753
#> 21                  Domineering  0.3650 0.416
#> 22                 Eccentricity  0.5838 0.594
#> 23                 Emotionality  0.7418 0.755
#> 24                  Entitlement  0.3712 0.403
#> 25           Excessive Exercise  0.5155 0.553
#> 26                  Excoriation  0.4598 0.487
#> 27                Exhibitionism  0.4215 0.441
#> 28            Fantasy Proneness  0.5749 0.592
#> 29             Food Selectivity  0.4096 0.431
#> 30                     Gambling  0.3668 0.440
#> 31                       Gaming  0.3686 0.372
#> 32                  Grandiosity  0.3269 0.345
#> 33               Health Anxiety  0.4414 0.457
#> 34                     Hoarding  0.5728 0.616
#> 35            Hyperdeliberation  0.2873 0.357
#> 36               Hypervigilance  0.6377 0.662
#> 37                     Insomnia  0.4203 0.440
#> 38           Low Sexual Arousal  0.3140 0.458
#> 39          Low Sexual Interest  0.6008 0.674
#> 40                 Manic Energy  0.3561 0.392
#> 41                     Mistrust  0.4914 0.527
#> 42              Muscle Building  0.3529 0.487
#> 43                         Nssi  0.4611 0.495
#> 44                   Nightmares  0.2203    NA
#> 45              Non Persistence  0.6345 0.672
#> 46              Non Planfulness  0.5382 0.561
#> 47              Oppositionality  0.3296 0.323
#> 48                        Panic  0.1326 0.244
#> 49                  Paraphilias  0.4004 0.392
#> 50                Perfectionism  0.5222 0.536
#> 51             Premature Orgasm  0.3370    NA
#> 52         Problematic Shopping  0.3732 0.411
#> 53                      Purging  0.2713 0.300
#> 54           Reality Distortion  0.6772 0.687
#> 55                 Restlessness  0.5616 0.598
#> 56       Restricted Affectivity  0.1401 0.153
#> 57            Restricted Eating  0.5266 0.576
#> 58                     Rigidity  0.3968 0.448
#> 59                Risk Aversion  0.5158 0.574
#> 60                  Risk Taking  0.5511 0.570
#> 61                    Risky Sex  0.1348 0.203
#> 62         Romantic Disinterest -0.0803 0.200
#> 63    Sex Related Substance Use  0.3688 0.403
#> 64              Sexual Distress  0.5135 0.526
#> 65                  Sexual Pain  0.4416 0.458
#> 66            Social Aggression  0.5386 0.555
#> 67             Social Aloofness  0.5550 0.599
#> 68               Social Anxiety  0.3877 0.482
#> 69        Somatic Preoccupation  0.5924 0.661
#> 70        Specific Phobia Index  0.5931 0.598
#> 71               Submissiveness  0.1241    NA
#> 72                  Suicidality  0.4398 0.466
#> 73             Trauma Reactions  0.3306 0.357
#> 74             Trichotillomania  0.1963    NA
#> 75                   Well Being  0.6629 0.680
#> 76                  Workaholism  0.4852 0.502
```
