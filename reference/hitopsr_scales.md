# HiTOP-SR Scale Data

Information about scales in the HiTOP-SR.

## Usage

``` r
hitopsr_scales
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 76
rows and 5 columns:

- Scale:

  Name of the scale

- itemdata:

  A list column containing one item-data tibble per scale; its
  item-number column is an integer

- nItems:

  The number of items in the scale (integer)

- itemNumbers:

  A list column containing one integer item-number vector per scale

- camelCase:

  The name of the scale converted to camel case

## Examples

``` r
hitopsr_scales
#> # A tibble: 76 × 5
#>    Scale                itemdata         nItems itemNumbers  camelCase          
#>    <chr>                <list>            <int> <named list> <chr>              
#>  1 Agoraphobia          <tibble [5 × 4]>      5 <int [5]>    agoraphobia        
#>  2 Antisocial Behavior  <tibble [8 × 4]>      8 <int [8]>    antisocialBehavior 
#>  3 Appearance Focus     <tibble [5 × 4]>      5 <int [5]>    appearanceFocus    
#>  4 Appetite Loss        <tibble [3 × 4]>      3 <int [3]>    appetiteLoss       
#>  5 Binge Eating         <tibble [3 × 4]>      3 <int [3]>    bingeEating        
#>  6 Bodily Distress      <tibble [6 × 4]>      6 <int [6]>    bodilyDistress     
#>  7 Body Dissatisfaction <tibble [4 × 4]>      4 <int [4]>    bodyDissatisfaction
#>  8 Callousness          <tibble [6 × 4]>      6 <int [6]>    callousness        
#>  9 Checking             <tibble [5 × 4]>      5 <int [5]>    checking           
#> 10 Cleaning             <tibble [6 × 4]>      6 <int [6]>    cleaning           
#> # ℹ 66 more rows
```
