# Personality Inventory for DSM-5 Scale Data

Information about the scales (facets) in different versions of the
PID-5, used by
[`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
to map each scale to its item numbers.

## Usage

``` r
pid_scales
```

## Format

A named [list](https://rdrr.io/r/base/list.html) of length 3 (elements
`FULL`, `SF`, and `BF`), one per PID-5 version. Each element is a
[tibble](https://tibble.tidyverse.org/reference/tibble.html) with one
row per scale and 5 columns:

- Facet (named `Domain` in the BF element):

  Name of the scale: the facet for the FULL and SF versions, the domain
  for the BF version

- itemdata:

  A list column containing one item-data tibble per scale

- nItems:

  The number of items in the scale

- itemNumbers:

  A list column containing one item-number vector per scale

- camelCase:

  The name of the scale converted to camel case (the score-output column
  stem)

## Examples

``` r
pid_scales[["BF"]]
#> # A tibble: 5 × 5
#>   Domain               itemdata         nItems itemNumbers  camelCase          
#>   <chr>                <list>            <dbl> <named list> <chr>              
#> 1 Disinhibition        <tibble [5 × 3]>      5 <dbl [5]>    disinhibition      
#> 2 Detachment           <tibble [5 × 3]>      5 <dbl [5]>    detachment         
#> 3 Psychoticism         <tibble [5 × 3]>      5 <dbl [5]>    psychoticism       
#> 4 Negative affectivity <tibble [5 × 3]>      5 <dbl [5]>    negativeAffectivity
#> 5 Antagonism           <tibble [5 × 3]>      5 <dbl [5]>    antagonism         
```
