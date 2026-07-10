# HiTOP-BR Item Data

Information about items in the HiTOP-BR.

## Usage

``` r
hitopbr_items
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 45
rows and 8 columns:

- HBR:

  Item number on the HITOP-BR

- Reverse:

  Whether the item needs to be reverse scored

- Scale:

  Name of the scale

- Externalizing:

  Whether the item is part of the Externalizing scale

- Pfactor:

  Whether the item is part of the p-Factor scale

- Text:

  Item text

- HSR:

  Item number on the HiTOP-SR

- Original:

  Item ID in the original, development item pool

## Examples

``` r
hitopbr_items
#> # A tibble: 45 × 8
#>      HBR Reverse Scale            Externalizing Pfactor Text        HSR Original
#>    <dbl> <lgl>   <chr>            <lgl>         <lgl>   <chr>     <dbl> <chr>   
#>  1     1 FALSE   Antagonism       TRUE          TRUE    I found …   303 Ext_22  
#>  2     2 FALSE   Antagonism       FALSE         FALSE   I deserv…    95 Ext_175 
#>  3     3 FALSE   Thought Disorder FALSE         FALSE   I saw th…     3 HiTOP_5…
#>  4     4 FALSE   Thought Disorder FALSE         FALSE   My fanta…   220 HiTOP_5…
#>  5     5 FALSE   Antagonism       FALSE         FALSE   I liked …    52 Ext_50  
#>  6     6 FALSE   Somatoform       FALSE         TRUE    I felt s…   115 HiTOP_4…
#>  7     7 FALSE   Detachment       FALSE         FALSE   When I h…    45 HiTOP_50
#>  8     8 FALSE   Internalizing    FALSE         FALSE   My moods…   231 HiTOP_5…
#>  9     9 FALSE   Internalizing    FALSE         FALSE   My mind …    94 HiTOP_3…
#> 10    10 FALSE   Somatoform       FALSE         FALSE   I had pa…    10 HiTOP_4…
#> # ℹ 35 more rows
```
