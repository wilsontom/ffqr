#' @rdname total_nutrients
#'
#' @description Extact the total nutrient profile from `ffqr` object

setMethod(f = total_nutrients, signature = 'ffqr',
          function(object)
          {
            nutrient_totals <- list()
            for (i in seq_along(object@Nutrients)) {
              nutrient_totals_tmp <-
                object@Nutrients[[i]] %>% dplyr::summarise_if(is.numeric, sum) %>% t()

              nutrient_totals[[i]] <-
                dplyr::tibble(NUTRIENT = rownames(nutrient_totals_tmp),
                              VALUE = nutrient_totals_tmp[, 1])

            }

            return(nutrient_totals)

          })
