#' Extract FFQ items for AHEI A10
#'
#' Extract all the nesscary FFQ items which are used to calculate a score for AHEI group A10 (Alcohol)
#'
#' @param x a `tibble` or `data.frame` of an FFQ
#' @return a numeric value for the number of drinks per day
#' @export

ExtractA10 <- function(x)
{
  search_terms <- c('Wine',
                    'Beer, lager or cider',
                    'Port, sherry, vermouth, liqueurs',
                    'Spirits, e.g. gin, brandy, whisky, vodka'
  )


  ext_list <- list()
  for (i in seq_along(search_terms)) {
    ext_list[[i]] <- str_detect(names(x), search_terms[i])
  }

  ext_pos <-  map(ext_list, ~ {
    which(. == TRUE)
  })

  A10pos <- unlist(ext_pos)

  A10x <- x[, A10pos] %>% t()

  A10xt <- tibble(field = rownames(A10x), value = A10x[, 1])

  A10out <- map(A10xt[, 'value'], check_value)$value

  if (length(A10out) > 0) {
    A10ext_final <- A10xt[-A10out, ]
  } else{
    A10ext_final <- A10xt
  }

  return(A10ext_final)

}
