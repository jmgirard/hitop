# HiTOP-DAT Scale Data

The scales the HiTOP-DAT scores, one row per scale: the 19 IDAS-II
scales, the 33 CAT-PD facets, and one total each for the WHODAS, AUDIT,
DUDIT, CAPE positive items and PHQ-15. `Scale` is the name the HiTOP-DAT
manual (2021) gives the scale. Item numbers are battery numbers, as in
`hitopdat_items$Item`.

## Usage

``` r
hitopdat_scales
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 57
rows and 6 columns:

- Measure:

  The measure the scale belongs to, as in `hitopdat_items$Measure`

- Scale:

  Name of the scale

- camelCase:

  The name of the scale converted to camel case

- itemNumbers:

  A list column containing one integer item-number vector per scale

- reverseNumbers:

  A list column containing, per scale, the integer numbers of the items
  that scale reverses (empty when it reverses none)

- nItems:

  The number of items in the scale (integer)

## Details

Scale membership and reverse keying come from the scoring of the
battery's Qualtrics file. The CAT-PD facets are checked against the IPIP
CAT-PD-SF v1.1 key, and the IDAS-II scales against the IDAS-II scoring
key (Watson, 2011). An item can be reversed in one scale and not in
another: the IDAS-II's General Depression reverses two Well-Being items
that Well-Being scores forward.

## See also

[hitopdat_items](https://jmgirard.github.io/hitop/reference/hitopdat_items.md)

## Examples

``` r
hitopdat_scales
#> # A tibble: 57 × 6
#>    Measure Scale              camelCase        itemNumbers reverseNumbers nItems
#>    <chr>   <chr>              <chr>            <named lis> <named list>    <int>
#>  1 WHODAS  WHODAS             whodas           <int [12]>  <int [0]>          12
#>  2 IDAS-II General Depression generalDepressi… <int [20]>  <int [2]>          20
#>  3 IDAS-II Dysphoria          dysphoria        <int [10]>  <int [0]>          10
#>  4 IDAS-II Lassitude          lassitude        <int [6]>   <int [0]>           6
#>  5 IDAS-II Insomnia           insomnia         <int [6]>   <int [0]>           6
#>  6 IDAS-II Suicidality        suicidality      <int [6]>   <int [0]>           6
#>  7 IDAS-II Appetite Loss      appetiteLoss     <int [3]>   <int [0]>           3
#>  8 IDAS-II Appetite Gain      appetiteGain     <int [3]>   <int [0]>           3
#>  9 IDAS-II Well-Being         wellBeing        <int [8]>   <int [0]>           8
#> 10 IDAS-II Ill Temper         illTemper        <int [5]>   <int [0]>           5
#> # ℹ 47 more rows
```
