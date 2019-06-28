#' @rdname calculate_nutrients
#'
#' @description Convert FFQ daily poritions to daily nutrient intake

setMethod(f = calculate_nutrients, signature = 'ffqr',
          function(object)
          {
            nutrient_match <- list()
            for (i in seq_along(object@FFQ)) {
              nutrient_match[[i]] <-
                component_matching(object@FFQ[[i]], Nutrients)

            }


            ffq_nutrients <- list()
            for (i in seq_along(object@FFQ)) {
              ffq_nutrients[[i]] <-
                object@FFQ[[i]]$GRAMS_SERVING[nutrient_match[[i]]$FFQ] * (Nutrients[nutrient_match[[i]]$INDEX, -1] / 100)

              ffq_nutrients[[i]] <-
                ffq_nutrients[[i]] %>% tibble::add_column(.,
                                                          FOOD_COMPONENT = object@FFQ[[i]]$FOOD_COMPONENT,
                                                          .before = 'KCALS')
            }


            object@Nutrients <- ffq_nutrients

            return(object)


          })
