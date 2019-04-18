#' Extract FFQ items for AHEI A1
#'
#' Extract all the nesscary FFQ items which are used to calculate a score for AHEI group A1 (Vegetables)
#'
#' @param x a `tibble` or `data.frame` of an FFQ
#' @return a numeric value for the total servings per day of vegetables
#' @export

ExtractA1 <- function(x)
{
  search_terms <- c(
    'Vegetable soups',
    'Tomato based sauces',
    'Carrots',
    'Spinach',
    'Broccoli',
    'Brussels sprouts',
    'Cabbage',
    'Peas',
    'Green beans, broad beans, runner beans',
    'Marrow, courgettes',
    'Cauliflower',
    'Parsnips, turnips, swedes',
    'Leeks',
    'Onions',
    'Garlic',
    'Mushrooms',
    'Sweet peppers',
    'Beansprouts',
    'Green salad, lettuce, cucumber, celery',
    'Mixed vegetables',
    'Watercress',
    'Tomatoes'
  )

  ext_list <- list()
  for (i in seq_along(search_terms)) {
    ext_list[[i]] <- str_detect(names(x), search_terms[i])
  }

  ext_pos <-  map(ext_list, ~ {
    which(. == TRUE)
  })

  A1pos <- unlist(ext_pos)

  A1x <- x[, A1pos] %>% t()

  A1xt <- tibble(field = rownames(A1x), value = A1x[, 1])


  A1out <- map(A1xt[, 'value'], check_value)$value

  A1ext_final <- A1xt[-A1out, ]

  remove_mismatches <- str_detect(A1ext_final$field, 'bread')

  rmm <- which(remove_mismatches == TRUE)

  if (length(rmm > 0)) {
    A1ext_final <- A1ext_final[-rmm, ]
  }

  return(A1ext_final)

}
