# HiTOP-DAT Answer Sets

The answer sets referenced by `hitopdat_items$Choice_Set`, one row per
answer. `Value` is the value the battery's Qualtrics file gives the
answer when it scores an item in the forward direction. The file also
offers a "Skip" answer on every item, which is not included.

## Usage

``` r
hitopdat_choices
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 58
rows and 3 columns:

- Choice_Set:

  Name of the answer set

- Value:

  Coded response value (integer)

- Label:

  Response label displayed to respondents

## See also

[hitopdat_items](https://jmgirard.github.io/hitop/reference/hitopdat_items.md)

## Examples

``` r
hitopdat_choices
#> # A tibble: 58 × 3
#>    Choice_Set Value Label            
#>    <chr>      <int> <chr>            
#>  1 whodas         0 None             
#>  2 whodas         1 Mild             
#>  3 whodas         2 Moderate         
#>  4 whodas         3 Severe           
#>  5 whodas         4 Extreme/Cannot do
#>  6 idas           1 Not at all       
#>  7 idas           2 A little bit     
#>  8 idas           3 Moderately       
#>  9 idas           4 Quite a bit      
#> 10 idas           5 Extremely        
#> # ℹ 48 more rows
```
