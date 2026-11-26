# HiTOP-DAT Norms

The mean and standard deviation that
[`norm_hitopdat()`](https://jmgirard.github.io/hitop/reference/norm_hitopdat.md)
converts each HiTOP-DAT scale score against, one row per normed scale.
The battery's authors named each source in their answers to the package
maintainer's questions in 2026, and this table takes each value from the
source they named:

## Usage

``` r
hitopdat_norms
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 53
rows and 8 columns:

- Measure:

  The measure the scale belongs to, as in `hitopdat_scales$Measure`

- Scale:

  Name of the scale, as in `hitopdat_scales$Scale`

- camelCase:

  The name of the scale converted to camel case, as in
  `hitopdat_scales$camelCase`

- M:

  The reference sample's mean

- SD:

  The reference sample's standard deviation

- Sample:

  The reference sample, as its source describes it

- N:

  The reference sample's size (integer), where the source prints one.
  `NA` for the CAT-PD facets, whose workbook gives none, and for the
  CAPE, whose source prints the men's and women's n (in `Sample`) but no
  total.

- Source:

  Where the mean and SD come from

## Details

- WHODAS: Andrews et al. (2009), Table 1, the total row and total column
  (N = 8,824).

- The 18 IDAS-II scales: Watson et al. (2012), Table 2, the "Adults"
  column (N = 1,091).

- CAPE positive symptoms: Galletly et al. (2016), Table 1. The mean is
  the mean of the men's 26.26 (n = 125) and the women's 26.58 (n = 180),
  weighted by their n, and the SD is the 3.52 printed for both. Both are
  divided by the 20 items, because
  [`score_hitopdat()`](https://jmgirard.github.io/hitop/reference/score_hitopdat.md)
  scores the CAPE as an item mean.

- The 33 CAT-PD facets: Len Simms's CAT-PD and IDAS scoring workbook,
  the "Community" mean and SD of its "CAT-PD Scoring" sheet, at the
  precision the workbook stores. These norms are unpublished. The
  battery's authors use them, and the package follows the authors.

Each mean and SD is on the metric
[`score_hitopdat()`](https://jmgirard.github.io/hitop/reference/score_hitopdat.md)
scores the scale on: a sum for the WHODAS and the IDAS-II, and an item
mean for the CAPE and the CAT-PD.

The IDAS-II General Depression scale has no row, because the authors
dropped it. The AUDIT, DUDIT and PHQ-15 have no row and get no T score.
The AUDIT and DUDIT norms are split by sex, and how to report them waits
on the authors' answer. The authors keep the PHQ-15 on its cutoff scores
rather than a T score.

[`norm_hitopdat()`](https://jmgirard.github.io/hitop/reference/norm_hitopdat.md)
also flags an IDAS-II Suicidality or CAT-PD Self-Harm T score of 65 or
more, the alert point Katherine Jonas gave in her email to the package
maintainer of 2026-10-15. She added that clinics may set another, so the
function takes it as an argument.

## References

Andrews, G., Kemp, A., Sunderland, M., Von Korff, M., & Ustun, T. B.
(2009). Normative data for the 12 item WHO Disability Assessment
Schedule 2.0. *PLoS ONE, 4*(12), e8343.
[doi:10.1371/journal.pone.0008343](https://doi.org/10.1371/journal.pone.0008343)

Galletly, C., Clark, L., McFarlane, A., Searle, A., Sawyer, M., Sim, M.,
Baghurst, P., & van Hooff, M. (2016). Childhood lead exposure, childhood
trauma, substance use and subclinical psychotic experiences: A
longitudinal cohort study. *Psychiatry Research, 239*, 54-61.
[doi:10.1016/j.psychres.2016.02.066](https://doi.org/10.1016/j.psychres.2016.02.066)

Simms, L. J. (n.d.). *CAT-PD and IDAS scoring* \[Unpublished Excel
workbook\]. Received by the package maintainer from K. G. Jonas,
2026-10-15.

Watson, D., O'Hara, M. W., Naragon-Gainey, K., Koffel, E., Chmielewski,
M., Kotov, R., Stasik, S. M., & Ruggero, C. J. (2012). Development and
validation of new anxiety and bipolar symptom scales for an expanded
version of the IDAS (the IDAS-II). *Assessment, 19*(4), 399-420.
[doi:10.1177/1073191112449857](https://doi.org/10.1177/1073191112449857)

## See also

[`norm_hitopdat()`](https://jmgirard.github.io/hitop/reference/norm_hitopdat.md),
[`score_hitopdat()`](https://jmgirard.github.io/hitop/reference/score_hitopdat.md),
[hitopdat_scales](https://jmgirard.github.io/hitop/reference/hitopdat_scales.md)

## Examples

``` r
hitopdat_norms
#> # A tibble: 53 × 8
#>    Measure Scale         camelCase        M    SD Sample                N Source
#>    <chr>   <chr>         <chr>        <dbl> <dbl> <chr>             <int> <chr> 
#>  1 WHODAS  WHODAS        whodas        3.1   5.3  2007 Australian …  8824 Andre…
#>  2 IDAS-II Dysphoria     dysphoria    19.4   7.57 Combined communi…  1091 Watso…
#>  3 IDAS-II Lassitude     lassitude    12.5   5.04 Combined communi…  1091 Watso…
#>  4 IDAS-II Insomnia      insomnia     11.5   4.97 Combined communi…  1091 Watso…
#>  5 IDAS-II Suicidality   suicidality   7.12  2.37 Combined communi…  1091 Watso…
#>  6 IDAS-II Appetite Loss appetiteLoss  4.61  2.21 Combined communi…  1091 Watso…
#>  7 IDAS-II Appetite Gain appetiteGain  6.14  2.88 Combined communi…  1091 Watso…
#>  8 IDAS-II Well-Being    wellBeing    23.4   6.71 Combined communi…  1091 Watso…
#>  9 IDAS-II Ill Temper    illTemper     7.87  3.73 Combined communi…  1091 Watso…
#> 10 IDAS-II Mania         mania         8.05  3.67 Combined communi…  1091 Watso…
#> # ℹ 43 more rows
```
