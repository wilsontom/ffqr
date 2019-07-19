#' Open Food Frequency Questionaire
#'
#' @param ffq a list of Food Frequency Questionaires (FFQ)
#' @return a S4 object (`ffqr`) which FFQ data converted to protions per day and grams per day
#'
#' @importFrom methods new
#' @importFrom  dplyr %>% left_join filter summarise mutate select summarise_each bind_rows
#' @importFrom purrr map_chr map2_dbl
#' @importFrom stringr str_split str_remove
#' @export


openFFQ <- function(ffq_df = list())
{

  ffq <- list()
  for(i in 1:nrow(ffq_df)){
    ffq[[i]] <- ffq_df[i,]
  }


  columns_out <- purrr::map(ffq, check_value)

  for (i in seq_along(ffq)) {
    if(length(columns_out[[i]]) > 0){
    ffq[[i]] <- ffq[[i]][, -columns_out[[i]]]
    }
  }

  FFQStranspose <- purrr::map(ffq, t)

  ffq_clean <- list()
  for (i in seq_along(FFQStranspose)) {
    ffq_clean[[i]] <-
      dplyr::tibble(FOOD_COMPONENT = rownames(FFQStranspose[[i]]),
                    FREQ = FFQStranspose[[i]][, 1])
  }


  # convert to servings

  ffq_servings <- list()
  for (i in seq_along(ffq_clean)) {
    ffq_servings[[i]] <-
      purrr::map(ffq_clean[[i]]$FREQ, convert_servings) %>% unlist()
  }


  ffq_clean2 <- list()
  for (i in seq_along(ffq_clean)) {
    ffq_clean2[[i]] <-
      ffq_clean[[i]] %>% mutate(SERVINGS = ffq_servings[[i]])
  }

  # match against portions

  ffq_match <- list()
  for (i in seq_along(ffq_clean2)) {
    ffq_match[[i]] <- component_matching(ffq_clean2[[i]], Portions)
  }


  for (i in seq_along(ffq_match)) {
    ffq_clean2[[i]] <-
      ffq_clean2[[i]] %>%
      mutate(GRAMS_SERVING = ffq_clean2[[i]]$SERVINGS[ffq_match[[i]]$FFQ] * Portions$EPIC[ffq_match[[i]]$INDEX])
  }


  object <- new('ffqr')

  object@FFQ <- ffq_clean2

  return(object)

}
