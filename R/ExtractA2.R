#' Extract FFQ items for AHEI A2
#'
#' Extract all the nesscary FFQ items which are used to calculate a score for AHEI group A2 (Fruit)
#'
#' @param x a `tibble` or `data.frame` of an FFQ
#' @return a numeric value for the total servings per day of fruit
#' @export

ExtractA2 <- function(x)
{
  search_terms <- c(
    'Apples',
    'Pears',
    'Oranges, satsumas, mandarins, tangerines, clementines',
    'Grapefruit',
    'Bananas',
    'Grapes',
    'Melon',
    'Peaches, plums, apricots, nectarines',
    'Strawberries, raspberries, kiwi fruit',
    'Tinned fruit',
    'Dried fruit, e.g. raisins, prunes, figs'
  )

  ext_list <- list()
  for (i in seq_along(search_terms)) {
    ext_list[[i]] <- str_detect(names(x), search_terms[i])
  }

  ext_pos <-  map(ext_list, ~ {
    which(. == TRUE)
  })

  A2pos <- unlist(ext_pos)

  A2x <- x[, A2pos] %>% t()

  A2xt <- tibble(field = rownames(A2x), value = A2x[, 1])


  A2out <- map(A2xt[, 'value'], check_value)$value

  if (length(A2out) > 0) {
    A2ext_final <- A2xt[-A2out,]
  } else{
    A2ext_final <- A2xt
  }

  return(A2ext_final)

}
