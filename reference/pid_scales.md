# Personality Inventory for DSM-5 Scale Data

Information about the scales (facets) in different versions of the
PID-5, used by
[`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
to map each scale to its item numbers. It is also read by
[`reliability_pid5()`](https://jmgirard.github.io/hitop/reference/reliability_pid5.md)
and by the printed scoring table in `generate_docx_pid5*()`, so adding
or removing a row changes all three.

## Usage

``` r
pid_scales
```

## Format

A named [list](https://rdrr.io/r/base/list.html) of length 6 (elements
`FULL`, `SF`, `BF`, `BFPM`, `IRF`, and `FFBF`), one per PID-5 version.
Each element is a
[tibble](https://tibble.tidyverse.org/reference/tibble.html) with one
row per scale and 5 columns:

- Facet (named `Domain` in the BF element):

  Name of the scale: the facet for the FULL, SF, BFPM, IRF, and FFBF
  versions, the domain for the BF version. The BF element carries a
  sixth row, `Total`, which is not a domain but the whole 25-item form
  scored as one scale (see
  [`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)).
  The BFPM element holds the 18 facets of the PID5BF+M, 2 items each,
  grouped by domain in the order of its key; its domains are in
  [pid_bfpm_domains](https://jmgirard.github.io/hitop/reference/pid_bfpm_domains.md).
  The IRF element holds the 25 facets of the Informant Form in the FULL
  element's order, numbered by informant item, with the informant text
  in `itemdata`. The FFBF element holds the 25 facets of the PID-5
  Forensic Faceted Brief Form in the SF element's order, numbered by
  FFBF item, with the English self-report text in `itemdata`; its items
  are in
  [pid_ffbf_items](https://jmgirard.github.io/hitop/reference/pid_ffbf_items.md)
  and its domains in
  [pid_ffbf_domains](https://jmgirard.github.io/hitop/reference/pid_ffbf_domains.md)

- itemdata:

  A list column containing one item-data tibble per scale; its
  item-number column is an integer

- nItems:

  The number of items in the scale (integer)

- itemNumbers:

  A list column containing one integer item-number vector per scale

- camelCase:

  The name of the scale converted to camel case (the score-output column
  stem)

## Examples

``` r
pid_scales[["BF"]]
#> # A tibble: 6 × 5
#>   Domain               itemdata          nItems itemNumbers  camelCase          
#>   <chr>                <list>             <int> <named list> <chr>              
#> 1 Disinhibition        <tibble [5 × 3]>       5 <int [5]>    disinhibition      
#> 2 Detachment           <tibble [5 × 3]>       5 <int [5]>    detachment         
#> 3 Psychoticism         <tibble [5 × 3]>       5 <int [5]>    psychoticism       
#> 4 Negative affectivity <tibble [5 × 3]>       5 <int [5]>    negativeAffectivity
#> 5 Antagonism           <tibble [5 × 3]>       5 <int [5]>    antagonism         
#> 6 Total                <tibble [25 × 3]>     25 <int [25]>   total              
```
