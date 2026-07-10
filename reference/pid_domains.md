# Personality Inventory for DSM-5 Domain Data

The map from each of the 5 PID-5 personality-trait domains to the 3
facets contributing primarily to it, used to compute domain scores for
the FULL and SF versions (APA scoring key Step 3). This is the 15-facet
primary subset, not the broader `pid_items$Domain` grouping.

## Usage

``` r
pid_domains
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 5
rows and 4 columns:

- Domain:

  Name of the domain (matches `pid_items$Domain`)

- camelCase:

  The domain name in camel case (the score-output column stem)

- primaryFacets:

  A list column of the 3 primary facet names per domain

- facetStems:

  A list column of those 3 facet names in camel case (the facet
  score-output column stems)

## Examples

``` r
pid_domains
#> # A tibble: 5 × 4
#>   Domain               camelCase           primaryFacets facetStems
#>   <chr>                <chr>               <list>        <list>    
#> 1 Negative affectivity negativeAffectivity <chr [3]>     <chr [3]> 
#> 2 Detachment           detachment          <chr [3]>     <chr [3]> 
#> 3 Antagonism           antagonism          <chr [3]>     <chr [3]> 
#> 4 Disinhibition        disinhibition       <chr [3]>     <chr [3]> 
#> 5 Psychoticism         psychoticism        <chr [3]>     <chr [3]> 
```
