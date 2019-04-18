#' Extract FFQ items for AHEI A5
#'
#' Extract all the nesscary FFQ items which are used to calculate a score for AHEI group A5 (Nuts and Legumes)
#'
#' @param x a `tibble` or `data.frame` of an FFQ
#' @return a numeric value for the total servings per day of nuts and legumes
#' @export

ExtractA5 <- function(x)
{
  search_terms <- c('Peanuts or other nuts',
                    'Peanut butter',
                    'Baked Beans',
                    'Dried lentils, beans, peas',
                    'Tofu, soya meat, TVP, Vegeburger'
  )

  ext_list <- list()
  for (i in seq_along(search_terms)) {
    ext_list[[i]] <- str_detect(names(x), search_terms[i])
  }

  ext_pos <-  map(ext_list, ~ {
    which(. == TRUE)
  })

  A5pos <- unlist(ext_pos)

  A5x <- x[, A5pos] %>% t()

  A5xt <- tibble(field = rownames(A5x), value = A5x[, 1])

  A5out <- map(A5xt[, 'value'], check_value)$value

  if (length(A5out) > 0) {
    A5ext_final <- A5xt[-A5out, ]
  } else{
    A5ext_final <- A5xt
  }

  return(A5ext_final)

}
