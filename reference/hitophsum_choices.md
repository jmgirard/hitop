# HiTOP-HSUM Choice Sets

Response choice sets referenced by `hitophsum_items$Choice_Set`. Used by
the HiTOP-HSUM instrument generators (e.g.
[`generate_redcap_hitophsum()`](https://jmgirard.github.io/hitop/reference/generate_redcap_hitophsum.md)).

## Usage

``` r
hitophsum_choices
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 42
rows and 3 columns:

- Choice_Set:

  Name of the choice set

- Value:

  Coded response value

- Label:

  Response label displayed to respondents

## Examples

``` r
hitophsum_choices
#> # A tibble: 185 × 3
#>    Choice_Set     Value Label                                                   
#>    <chr>          <dbl> <chr>                                                   
#>  1 yn_pnts            1 Yes                                                     
#>  2 yn_pnts            0 No                                                      
#>  3 yn_pnts           99 Prefer not to say                                       
#>  4 nicotine_forms     1 smoking cigarettes                                      
#>  5 nicotine_forms     2 vaping or e-cigarettes                                  
#>  6 nicotine_forms     3 cigars, cigarillos, or filtered cigars filled only with…
#>  7 nicotine_forms     4 nicotine gum or patches                                 
#>  8 nicotine_forms     5 chewing tobacco or pouches/dip                          
#>  9 nicotine_forms     6 Other - specify:                                        
#> 10 freq_12m           1 1-5 days total                                          
#> # ℹ 175 more rows
```
