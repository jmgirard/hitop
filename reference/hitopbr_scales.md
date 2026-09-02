# HiTOP-BR Scale Data

Information about scales in the HiTOP-BR.

## Usage

``` r
hitopbr_scales
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 8
rows and 5 columns:

- Scale:

  Name of the scale

- itemdata:

  A list column containing one item-data tibble per scale; its two
  item-number columns are integers

- nItems:

  The number of items in the scale (integer)

- itemNumbers:

  A list column containing one integer item-number vector per scale

- camelCase:

  The name of the scale converted to camel case

## Examples

``` r
hitopbr_scales
#> # A tibble: 8 × 5
#>   Scale            itemdata          nItems itemNumbers  camelCase      
#>   <chr>            <list>             <int> <named list> <chr>          
#> 1 Antagonism       <tibble [9 × 5]>       9 <int [9]>    antagonism     
#> 2 Detachment       <tibble [5 × 5]>       5 <int [5]>    detachment     
#> 3 Disinhibition    <tibble [9 × 5]>       9 <int [9]>    disinhibition  
#> 4 Internalizing    <tibble [8 × 5]>       8 <int [8]>    internalizing  
#> 5 Somatoform       <tibble [8 × 5]>       8 <int [8]>    somatoform     
#> 6 Thought Disorder <tibble [6 × 5]>       6 <int [6]>    thoughtDisorder
#> 7 Externalizing    <tibble [10 × 5]>     10 <int [10]>   externalizing  
#> 8 p-Factor         <tibble [12 × 5]>     12 <int [12]>   pFactor        
```
