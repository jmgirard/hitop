# HiTOP-SR Item Data

Information about items in the HiTOP-SR.

## Usage

``` r
hitopsr_items
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 405
rows and 6 columns:

- HSR:

  Item number on the full HiTOP-SR (integer)

- Reverse:

  Whether the item needs to be reverse scored

- Scale:

  Name of the scale (level 2)

- Subscale:

  Name of the subscale (level 1)

- Text:

  Item text

- Original:

  Item ID in the original, development item pool

## Details

Two scales carry other names in the literature and in earlier versions
of this package. The scale this table calls `Non-suicidal Self-injury`
is widely written by its abbreviation, **NSSI**, and was named that way
here before version 0.2.0; the scale it calls `Appearance Focus` was
named **Body Focus** here before version 0.2.0. Both names are the ones
printed in the HiTOP-SR introduction paper's Table 1. Scoring functions
derive a column name from each scale name, so those scales' scored
columns are `hsr_nonSuicidalSelfInjury` and `hsr_appearanceFocus`.

## Examples

``` r
hitopsr_items
#> # A tibble: 405 × 6
#>      HSR Reverse Scale              Subscale       Text                 Original
#>    <int> <lgl>   <chr>              <chr>          <chr>                <chr>   
#>  1     1 FALSE   Social Aloofness   NA             I preferred to stay… HiTOP_6…
#>  2     2 FALSE   Perfectionism      NA             I felt that my work… HiTOP_3…
#>  3     3 FALSE   Reality Distortion Hallucinations I saw things that w… HiTOP_5…
#>  4     4 FALSE   Trichotillomania   NA             I was embarrassed b… HiTOP_3…
#>  5     5 FALSE   Food Selectivity   NA             I avoided foods bec… HiTOP_81
#>  6     6 FALSE   Panic              NA             I felt dizzy or lig… HiTOP_2…
#>  7     7 FALSE   Dishonesty         Deceitfulness  I believed it was f… Ext_432 
#>  8     8 FALSE   Mistrust           Cynicism       Most people were ju… Ext_79  
#>  9     9 FALSE   Hoarding           NA             My home was so clut… HiTOP_2…
#> 10    10 FALSE   Bodily Distress    NA             I had pains in seve… HiTOP_4…
#> # ℹ 395 more rows
```
