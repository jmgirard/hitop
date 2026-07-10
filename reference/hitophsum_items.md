# HiTOP-HSUM Item Data

Information about the items in the HiTOP-HSUM (Harmful Substance Use
Measure). Used by the HiTOP-HSUM instrument generators (e.g.
[`generate_redcap_hitophsum()`](https://jmgirard.github.io/hitop/reference/generate_redcap_hitophsum.md)).

## Usage

``` r
hitophsum_items
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 650
rows and 9 columns:

- Item:

  Item number

- Variable:

  Variable name for the item

- Substance:

  Name of the substance the item refers to

- Tier:

  The assessment tier the item belongs to (e.g. Screening)

- Field_Type:

  The response field type (e.g. radio)

- Gate_Variable:

  Name of the gating variable, or NA if ungated

- Gate_Value:

  Value of the gating variable required to show the item, or NA

- Choice_Set:

  Name of the response choice set (see `hitophsum_choices`)

- Text:

  Item text

## Examples

``` r
hitophsum_items
#> # A tibble: 650 × 9
#>     Item Variable Substance Tier  Field_Type Gate_Variable Gate_Value Choice_Set
#>    <dbl> <chr>    <chr>     <chr> <chr>      <chr>         <chr>      <chr>     
#>  1     1 hsum_alc Alcohol   Scre… radio      NA            NA         yn_pnts   
#>  2     2 hsum_can Cannabis  Scre… radio      NA            NA         yn_pnts   
#>  3     3 hsum_nic Nicotine  Scre… radio      NA            NA         yn_pnts   
#>  4     4 hsum_coc Cocaine   Scre… radio      NA            NA         yn_pnts   
#>  5     5 hsum_stm Prescrip… Scre… radio      NA            NA         yn_pnts   
#>  6     6 hsum_met Methamph… Scre… radio      NA            NA         yn_pnts   
#>  7     7 hsum_inh Inhalants Scre… radio      NA            NA         yn_pnts   
#>  8     8 hsum_sed Sedatives Scre… radio      NA            NA         yn_pnts   
#>  9     9 hsum_hal Hallucin… Scre… radio      NA            NA         yn_pnts   
#> 10    10 hsum_sop Street o… Scre… radio      NA            NA         yn_pnts   
#> # ℹ 640 more rows
#> # ℹ 1 more variable: Text <chr>
```
