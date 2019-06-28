#' Component Matching
#'
#' Match FFQ items into specified index tables (Portions & Nutrients)
#'
#' @param clean_ffq a FFQ tibble which have been cleaned
#' @param index an index `tibble` containing a `FOOD_COMPONENT` column
#' @return a `tibble` of matcing indicies
#' @keywords internal

component_matching <- function(clean_ffq, index)
{
  component_grep <- list()
  for (i in seq_along(clean_ffq$FOOD_COMPONENT)) {
    component_grep[[i]] <-
      agrep(clean_ffq$FOOD_COMPONENT[i],
            index$FOOD_COMPONENT,
            max.distance = 0.15)
  }


  component_match <- list()

  for (i in seq_along(component_grep)) {
    if (length(component_grep[[i]]) == 1) {
      component_match[[i]] <- component_grep[[i]]

    } else{
      cosine_dist <- list()
      for (k in seq_along(component_grep[[i]])) {
        cosine_dist[[k]] <-
          stringdist::stringdist(clean_ffq$FOOD_COMPONENT[[i]],
                                 index$FOOD_COMPONENT[component_grep[[i]][[k]]],
                                 method = 'cosine')

        mindist <-
          which(unlist(cosine_dist) == min(unlist(cosine_dist)))

        component_match[[i]] <- component_grep[[i]][mindist]

      }

    }

  }

  match_tibble <-
    dplyr::tibble(FFQ = 1:length(component_match),
                  INDEX = unlist(component_match))

  return(match_tibble)

}
