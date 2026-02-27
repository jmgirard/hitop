# Scoring the PID-5

The Personality Inventory for DSM-5 (PID-5) instrument has 220 items and
yields 25 facet scales, 5 domain scales, and 5 validity scales. We can
demonstrate the package’s functionality using some simulated data.

First, we load the package into memory using the
[`library()`](https://rdrr.io/r/base/library.html) function. If this
doesn’t work, make sure you installed the package properly (see the
README on [GitHub](https://github.com/jmgirard/hitop)).

``` r
library(hitop)
```

### Score simulated PID-5 data

The `sim_pid5` dataset is built into the package and can be loaded using
the [`data()`](https://rdrr.io/r/utils/data.html) function. It contains
100 rows (each representing a simulated participant) and 220 columns
named `pid_1` to `pid_220` (each representing an item from the PID-5).

``` r
data("sim_pid5")
sim_pid5
#> # A tibble: 100 × 220
#>    pid_1 pid_2 pid_3 pid_4 pid_5 pid_6 pid_7 pid_8 pid_9 pid_10 pid_11 pid_12
#>    <int> <int> <int> <int> <int> <int> <int> <int> <int>  <int>  <int>  <int>
#>  1     0     3     2     1     1     3     1     3     3      0      0      3
#>  2     3     3     0     3     0     3     2     2     1      2      0      0
#>  3     3     2     3     2     3     3     0     3     3      2      3      3
#>  4     1     3     0     2     1     0     2     0     3      2      3      2
#>  5     0     1     3     2     3     1     0     1     2      2      2      2
#>  6     2     1     1     3     3     2     2     0     1      3      1      3
#>  7     1     1     3     3     1     3     1     0     1      1      0      2
#>  8     2     0     3     0     3     2     0     1     3      1      2      0
#>  9     1     1     3     0     1     1     2     3     1      1      3      1
#> 10     0     3     2     3     3     0     1     2     1      3      0      2
#> # ℹ 90 more rows
#> # ℹ 208 more variables: pid_13 <int>, pid_14 <int>, pid_15 <int>, pid_16 <int>,
#> #   pid_17 <int>, pid_18 <int>, pid_19 <int>, pid_20 <int>, pid_21 <int>,
#> #   pid_22 <int>, pid_23 <int>, pid_24 <int>, pid_25 <int>, pid_26 <int>,
#> #   pid_27 <int>, pid_28 <int>, pid_29 <int>, pid_30 <int>, pid_31 <int>,
#> #   pid_32 <int>, pid_33 <int>, pid_34 <int>, pid_35 <int>, pid_36 <int>,
#> #   pid_37 <int>, pid_38 <int>, pid_39 <int>, pid_40 <int>, pid_41 <int>, …
```

To turn these item-level data into scale scores on the 25 facets and 5
domains, we can use the
[`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
function. We will need to tell the function which columns contain our
items and which version of the PID this is. There are several ways we
can specify the items. First, we can provide the column numbers and use
the `:` shortcut. In this tibble, the items are from column 1 to column
220 so we can use `items = 1:220`. I am going to also set
`append = FALSE` so that you can quickly see the scale scores. I also
can set the version to `"FULL"` (or leave that argument off, as that is
the default, shown in the example example) to let it know we are using
the full 220-item version.

``` r
scores <- score_pid5(sim_pid5, items = 1:220, version = "FULL", append = FALSE)
scores
#> # A tibble: 100 × 25
#>    pid_anhedonia pid_suspiciousness pid_riskTaking pid_impulsivity
#>            <dbl>              <dbl>          <dbl>           <dbl>
#>  1          1.25               1.71           1.36           2.33 
#>  2          1.38               1.57           1.43           2    
#>  3          1.88               1              1.29           1.83 
#>  4          1.25               2.43           1.21           1.5  
#>  5          1.12               1.57           1.64           2.5  
#>  6          2.12               1              1.79           1.83 
#>  7          1.38               1.14           1.86           1.17 
#>  8          1.5                1.71           1.86           0.667
#>  9          1.12               1.14           1.86           1.67 
#> 10          1.38               1.86           2.07           2    
#> # ℹ 90 more rows
#> # ℹ 21 more variables: pid_eccentricity <dbl>, pid_distractibility <dbl>,
#> #   pid_restrictedAffectivity <dbl>, pid_submissiveness <dbl>,
#> #   pid_withdrawal <dbl>, pid_callousness <dbl>,
#> #   pid_separationInsecurity <dbl>, pid_attentionSeeking <dbl>,
#> #   pid_emotionalLability <dbl>, pid_depressivity <dbl>, pid_hostility <dbl>,
#> #   pid_irresponsibility <dbl>, pid_rigidPerfectionism <dbl>, …
```

If I had instead set `append = TRUE` (or left it off, as that is the
default), we would get back the `sim_pid5` tibble with the scale scores
added to the end as extra columns. Notice below how we now have 245
columns instead of 220 or 25.

``` r
scores <- score_pid5(sim_pid5, items = 1:220)
scores
#> # A tibble: 100 × 245
#>    pid_1 pid_2 pid_3 pid_4 pid_5 pid_6 pid_7 pid_8 pid_9 pid_10 pid_11 pid_12
#>    <int> <int> <int> <int> <int> <int> <int> <int> <int>  <int>  <int>  <int>
#>  1     0     3     2     1     1     3     1     3     3      0      0      3
#>  2     3     3     0     3     0     3     2     2     1      2      0      0
#>  3     3     2     3     2     3     3     0     3     3      2      3      3
#>  4     1     3     0     2     1     0     2     0     3      2      3      2
#>  5     0     1     3     2     3     1     0     1     2      2      2      2
#>  6     2     1     1     3     3     2     2     0     1      3      1      3
#>  7     1     1     3     3     1     3     1     0     1      1      0      2
#>  8     2     0     3     0     3     2     0     1     3      1      2      0
#>  9     1     1     3     0     1     1     2     3     1      1      3      1
#> 10     0     3     2     3     3     0     1     2     1      3      0      2
#> # ℹ 90 more rows
#> # ℹ 233 more variables: pid_13 <int>, pid_14 <int>, pid_15 <int>, pid_16 <int>,
#> #   pid_17 <int>, pid_18 <int>, pid_19 <int>, pid_20 <int>, pid_21 <int>,
#> #   pid_22 <int>, pid_23 <int>, pid_24 <int>, pid_25 <int>, pid_26 <int>,
#> #   pid_27 <int>, pid_28 <int>, pid_29 <int>, pid_30 <int>, pid_31 <int>,
#> #   pid_32 <int>, pid_33 <int>, pid_34 <int>, pid_35 <int>, pid_36 <int>,
#> #   pid_37 <int>, pid_38 <int>, pid_39 <int>, pid_40 <int>, pid_41 <int>, …
```

Alternatively, we could provide the item column names as a character
string. Typing out all 220 item names would be a hassle, but luckily
this data named them consistently so we can build the names
automatically using [`sprintf()`](https://rdrr.io/r/base/sprintf.html).
If we use the “pid\_%d” format and apply that across the numbers 1 to
220, that will create the column names we need.

``` r
scores <- score_pid5(sim_pid5, items = sprintf("pid_%d", 1:220))
scores
#> # A tibble: 100 × 245
#>    pid_1 pid_2 pid_3 pid_4 pid_5 pid_6 pid_7 pid_8 pid_9 pid_10 pid_11 pid_12
#>    <int> <int> <int> <int> <int> <int> <int> <int> <int>  <int>  <int>  <int>
#>  1     0     3     2     1     1     3     1     3     3      0      0      3
#>  2     3     3     0     3     0     3     2     2     1      2      0      0
#>  3     3     2     3     2     3     3     0     3     3      2      3      3
#>  4     1     3     0     2     1     0     2     0     3      2      3      2
#>  5     0     1     3     2     3     1     0     1     2      2      2      2
#>  6     2     1     1     3     3     2     2     0     1      3      1      3
#>  7     1     1     3     3     1     3     1     0     1      1      0      2
#>  8     2     0     3     0     3     2     0     1     3      1      2      0
#>  9     1     1     3     0     1     1     2     3     1      1      3      1
#> 10     0     3     2     3     3     0     1     2     1      3      0      2
#> # ℹ 90 more rows
#> # ℹ 233 more variables: pid_13 <int>, pid_14 <int>, pid_15 <int>, pid_16 <int>,
#> #   pid_17 <int>, pid_18 <int>, pid_19 <int>, pid_20 <int>, pid_21 <int>,
#> #   pid_22 <int>, pid_23 <int>, pid_24 <int>, pid_25 <int>, pid_26 <int>,
#> #   pid_27 <int>, pid_28 <int>, pid_29 <int>, pid_30 <int>, pid_31 <int>,
#> #   pid_32 <int>, pid_33 <int>, pid_34 <int>, pid_35 <int>, pid_36 <int>,
#> #   pid_37 <int>, pid_38 <int>, pid_39 <int>, pid_40 <int>, pid_41 <int>, …
```

There are other useful arguments to the function that you can read about
using its documentation by typing the following into your R console:
[`?score_pid5`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
or through the [package
website](https://jmgirard.github.io/hitop/reference/score_pid5.html).

### Validity Scales for the PID-5

There are also several validity scales that have been developed for the
full PID-5, including measures of overreporting, inconsistent
responding, and positive impression management. We can use the simulated
data to demonstrate the ability of the
[`validity_pid5()`](https://jmgirard.github.io/hitop/reference/validity_pid5.md)
function to calculate these scores and flag issues. The function
arguments will be consistent with what we just learned. Note that,
because the data is fake, we would expect there to be lots of validity
issues.

``` r
validity_pid5(sim_pid5, items = 1:220, append = FALSE)
#> ! A total of 99 observations (99.0%) met criteria for inconsistent responding on the INC (0 missing).
#> ℹ Consider removing them with `dplyr::filter(df, pid_INC < 17)`
#> ! A total of 53 observations (53.0%) met criteria for overreporting on the ORS (0 missing).
#> ℹ Consider removing them with `dplyr::filter(df, pid_ORS < 3)`
#> ! A total of 92 observations (92.0%) met criteria for defensiveness on the SDTD (0 missing).
#> ℹ Consider removing them with `dplyr::filter(df, pid_SDTD < 19)`
#> # A tibble: 100 × 5
#>    pid_PNA pid_INC pid_ORS pid_PRD pid_SDTD
#>      <dbl>   <dbl>   <dbl>   <dbl>    <dbl>
#>  1       0      25       2      40       26
#>  2       0      18       2      34       31
#>  3       0      32       2      34       29
#>  4       0      29       3      34       31
#>  5       0      24       0      36       17
#>  6       0      23       2      35       36
#>  7       0      42       2      31       19
#>  8       0      17       1      28       21
#>  9       0      25       4      41       24
#> 10       0      30       5      31       29
#> # ℹ 90 more rows
```
