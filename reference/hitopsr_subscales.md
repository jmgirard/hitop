# HiTOP-SR Subscale Data

Information about subscales in the HiTOP-SR.

## Usage

``` r
hitopsr_subscales
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 17
rows and 6 columns:

- Subscale:

  Name of the subscale

- Scale:

  Name of the scale that the subscale is part of

- itemdata:

  A list column containing one item-data tibble per subscale

- nItems:

  The number of items in the subscale

- itemNumbers:

  A list column containing one item-number vector per subscale

- camelCase:

  The name of the subscale converted to camel case

## Examples

``` r
hitopsr_subscales
#> # A tibble: 17 × 6
#>    Subscale               Scale            itemdata nItems itemNumbers camelCase
#>    <chr>                  <chr>            <list>    <dbl> <named lis> <chr>    
#>  1 Affective Lability     Emotionality     <tibble>      3 <dbl [3]>   affectiv…
#>  2 Angry Hostility        Emotionality     <tibble>      4 <dbl [4]>   angryHos…
#>  3 Anhedonia              Distress-Dyspho… <tibble>      3 <dbl [3]>   anhedonia
#>  4 Animal-Insect Phobia   Specific Phobia… <tibble>      5 <dbl [5]>   animalIn…
#>  5 Anxious Worry          Distress-Dyspho… <tibble>      3 <dbl [3]>   anxiousW…
#>  6 Blood-Injection Phobia Specific Phobia… <tibble>      3 <dbl [3]>   bloodInj…
#>  7 Cynicism               Mistrust         <tibble>      4 <dbl [4]>   cynicism 
#>  8 Deceitfulness          Dishonesty       <tibble>      4 <dbl [4]>   deceitfu…
#>  9 Delusions              Reality Distort… <tibble>      5 <dbl [5]>   delusions
#> 10 Depressed Mood         Distress-Dyspho… <tibble>      4 <dbl [4]>   depresse…
#> 11 Hallucinations         Reality Distort… <tibble>      6 <dbl [6]>   hallucin…
#> 12 Irritability           Emotionality     <tibble>      4 <dbl [4]>   irritabi…
#> 13 Lassitude              Distress-Dyspho… <tibble>      3 <dbl [3]>   lassitude
#> 14 Manipulativeness       Dishonesty       <tibble>      4 <dbl [4]>   manipula…
#> 15 Shame/Guilt            Distress-Dyspho… <tibble>      3 <dbl [3]>   shameGui…
#> 16 Situational Phobias    Specific Phobia… <tibble>      4 <dbl [4]>   situatio…
#> 17 Suspiciousness         Mistrust         <tibble>      4 <dbl [4]>   suspicio…
```
