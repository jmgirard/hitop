# PID5BF+M Domain Data

The map from each of the 6 domains of the PID5BF+M (the 36-item modified
brief form of the PID-5) to its 3 facets, used to compute the domain
scores of `score_pid5(version = "BFPM")` and the domain rows of
`reliability_pid5(version = "BFPM")`. Each domain score is the mean of
its 3 facet scores (Bach et al., 2020). The rows are in the order of the
form's key, with Anankastia fifth.

## Usage

``` r
pid_bfpm_domains
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 6
rows and 4 columns, the columns of
[pid_domains](https://jmgirard.github.io/hitop/reference/pid_domains.md):

- Domain:

  Name of the domain. The five domains the form shares with the PID-5
  are spelled as in
  [pid_domains](https://jmgirard.github.io/hitop/reference/pid_domains.md)

- camelCase:

  The domain name in camel case (the score-output column stem)

- primaryFacets:

  A list column of the 3 facet names per domain, as
  `pid_scales$BFPM$Facet` spells them

- facetStems:

  A list column of those 3 facet names in camel case (the facet
  score-output column stems)

## Source

Bach, B., Kerber, A., Aluja, A., Bastiaens, T., Keeley, J. W., Claes,
L., Fossati, A., Gutierrez, F., Oliveira, S. E. S., Pires, R., Riegel,
K. D., Rolland, J.-P., Roskam, I., Sellbom, M., Somma, A., Spanemberg,
L., Strus, W., Thimm, J. C., Wright, A. G. C., & Zimmermann, J. (2020).
International assessment of DSM-5 and ICD-11 personality disorder
traits: Toward a common nosology in DSM-5.1. *Psychopathology, 53*(3-4),
179-188. [doi:10.1159/000507589](https://doi.org/10.1159/000507589)

The keying itself is transcribed from the form's coding scheme: Kerber,
A. (2020). *Persönlichkeitsinventar für DSM-5 und ICD-11: Kurzform
Modifiziert (PID5BF+ M)* \[Questionnaire and coding scheme, German\].
Freie Universität Berlin, p. 2.

## Examples

``` r
pid_bfpm_domains
#> # A tibble: 6 × 4
#>   Domain               camelCase           primaryFacets facetStems
#>   <chr>                <chr>               <list>        <list>    
#> 1 Negative affectivity negativeAffectivity <chr [3]>     <chr [3]> 
#> 2 Detachment           detachment          <chr [3]>     <chr [3]> 
#> 3 Antagonism           antagonism          <chr [3]>     <chr [3]> 
#> 4 Disinhibition        disinhibition       <chr [3]>     <chr [3]> 
#> 5 Anankastia           anankastia          <chr [3]>     <chr [3]> 
#> 6 Psychoticism         psychoticism        <chr [3]>     <chr [3]> 
```
