# PID-5 Forensic Faceted Brief Form Domain Data

The map from each of the 7 domains that `score_pid5(version = "FFBF")`
returns to its 3 facets. The first 5 rows are the APA domains of
[pid_domains](https://jmgirard.github.io/hitop/reference/pid_domains.md),
each the mean of its 3 primary facets (Niemeyer et al., 2022, p. 33).
The last 2 rows are the domains of the paper's four-factor solution that
are not APA domains, Disinhibited Aggression and Insecurity, with the
facets of the authors' analysis code. The paper's four-factor Antagonism
and Detachment have the facets of the APA domains of the same names, so
they are not repeated.

## Usage

``` r
pid_ffbf_domains
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 7
rows and 4 columns, the columns of
[pid_domains](https://jmgirard.github.io/hitop/reference/pid_domains.md):

- Domain:

  Name of the domain. The APA domains are spelled as in
  [pid_domains](https://jmgirard.github.io/hitop/reference/pid_domains.md)
  and the two forensic domains as the paper prints them

- camelCase:

  The domain name in camel case (the score-output column stem)

- primaryFacets:

  A list column of the 3 facet names per domain

- facetStems:

  A list column of those 3 facet names in camel case (the facet
  score-output column stems)

## Source

Niemeyer, L. M., Grosz, M. P., Zimmermann, J., & Back, M. D. (2022).
*Journal of Personality Assessment, 104*(1), 30-43.
[doi:10.1080/00223891.2021.1923522](https://doi.org/10.1080/00223891.2021.1923522)
. The four-factor domain facets are from the authors' analysis code on
the paper's OSF project (<https://osf.io/m42gn/>).

## Examples

``` r
pid_ffbf_domains
#> # A tibble: 7 × 4
#>   Domain                  camelCase              primaryFacets facetStems
#>   <chr>                   <chr>                  <list>        <list>    
#> 1 Negative affectivity    negativeAffectivity    <chr [3]>     <chr [3]> 
#> 2 Detachment              detachment             <chr [3]>     <chr [3]> 
#> 3 Antagonism              antagonism             <chr [3]>     <chr [3]> 
#> 4 Disinhibition           disinhibition          <chr [3]>     <chr [3]> 
#> 5 Psychoticism            psychoticism           <chr [3]>     <chr [3]> 
#> 6 Disinhibited Aggression disinhibitedAggression <chr [3]>     <chr [3]> 
#> 7 Insecurity              insecurity             <chr [3]>     <chr [3]> 
```
