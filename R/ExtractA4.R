#' Extract FFQ items for AHEI A4
#'
#' Extract all the nesscary FFQ items which are used to calculate a score for AHEI group A4 (Sugar & Sweetend Drinks)
#'
#' @param x a `tibble` or `data.frame` of an FFQ
#' @return a numeric value for the total servings per day of sugar and sweetend drinks
#' @export

ExtractA4 <- function(x)
{
  search_terms <- c('Fizzy soft drinks',
                    'Pure fruit juice',
                    'Fruit squash or cordial')

  ext_list <- list()
  for (i in seq_along(search_terms)) {
    ext_list[[i]] <- str_detect(names(x), search_terms[i])
  }

  ext_pos <-  map(ext_list, ~ {
    which(. == TRUE)
  })

  A4pos <- unlist(ext_pos)

  A4x <- x[, A4pos] %>% t()

  A4xt <- tibble(field = rownames(A4x), value = A4x[, 1])


  A4out <- map(A4xt[, 'value'], check_value)$value

  if (length(A4out) > 0) {
    A4ext_final <- A4xt[-A4out, ]
  } else{
    A4ext_final <- A4xt
  }

  return(A4ext_final)

}
