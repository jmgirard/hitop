library(tidyverse)

hitop_pro_scales <- 
  hitop_pro_items |> 
  tidyr::nest(
    itemdata = c(PRO, Reverse, Text, Subscale), 
    .by = c(Scale, Subfactor, Spectrum)
  ) |> 
  dplyr::mutate(
    nItems = purrr::map_dbl(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "PRO"),
    camelCase = snakecase::to_any_case(Scale, case = "lower_camel")
  )
names(hitop_pro_scales$itemNumbers) <- hitop_pro_scales$Scale
usethis::use_data(hitop_pro_scales, overwrite = TRUE)

# ------------------------------------------------------------------------------

hitop_pro_subscales <- 
  hitop_pro_items |> 
  dplyr::filter(Subscale != "") |> 
  tidyr::nest(
    itemdata = c(PRO, Reverse, Text, Subscale), 
    .by = c(Subscale, Scale, Subfactor, Spectrum)
  ) |> 
  dplyr::mutate(
    nItems = purrr::map_dbl(itemdata, nrow),
    itemNumbers = purrr::map(itemdata, "PRO"),
    camelCase = snakecase::to_any_case(Subscale, case = "lower_camel")
  )
names(hitop_pro_subscales$itemNumbers) <- hitop_pro_subscales$Subscale
usethis::use_data(hitop_pro_subscales, overwrite = TRUE)

