#' Extract FFQ items for AHEI A6
#'
#' Extract all the nesscary FFQ items which are used to calculate a score for AHEI group A6 (Red and Processed Meat)
#'
#' @param x a `tibble` or `data.frame` of an FFQ
#' @return a numeric value for the total servings per day of red and processed meat
#' @export

ExtractA6 <- function(x)
{
  search_terms <- c('Beef: roast, steak, mince, stew casserole, curry or bolognese',
                    'Beefburgers',
                    'Pork: roast, chops, stew, slice or curry',
                    'Lamb: roast, chops, stew or curry',
                    'Bacon',
                    'Ham',
                    'Corned beef, Spam, luncheon meats',
                    'Sausages',
                    'Savoury pies, e.g. meat pie, pork pie, pasties, steak & kidney pie, sausage rolls, scotch egg',
                    'Liver, liver pate, liver sausage',
                    'Meat soups'
                    )


  ext_list <- list()
  for (i in seq_along(search_terms)) {
    ext_list[[i]] <- str_detect(names(x), search_terms[i])
  }

  ext_pos <-  map(ext_list, ~ {
    which(. == TRUE)
  })

  A6pos <- unlist(ext_pos)

  A6x <- x[, A6pos] %>% t()

  A6xt <- tibble(field = rownames(A6x), value = A6x[, 1])

  A6out <- map(A6xt[, 'value'], check_value)$value

  if (length(A6out) > 0) {
    A6ext_final <- A6xt[-A6out, ]
  } else{
    A6ext_final <- A6xt
  }

  return(A6ext_final)

}
