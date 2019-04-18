#' Extract FFQ items for AHEI A3
#'
#' Extract all the nesscary FFQ items which are used to calculate a score for AHEI group A3 (Cereal Fibre)
#'
#' @param x a `tibble` or `data.frame` of an FFQ
#' @return a numeric value for the total gramms per day of cereal fibre
#' @export

ExtractA3 <- function(x)
{
  search_terms <- c(
    'Brown bread and rolls',
    'Wholemeal bread and rolls',
    'Porridge, Readybrek',
    'All Bran, Bran Flakes, Muesli',
    'Wholegrain cereals e.g. Cheerios, Weetabix, Shredded Wheat',
    'Brown rice',
    'Wholemeal pasta'
  )

  ext_list <- list()
  for (i in seq_along(search_terms)) {
    ext_list[[i]] <- str_detect(names(x), search_terms[i])
  }

  ext_pos <-  map(ext_list, ~ {
    which(. == TRUE)
  })

  A3pos <- unlist(ext_pos)

  A3x <- x[, A3pos] %>% t()

  A3xt <- tibble(field = rownames(A3x), value = A3x[, 1])


  A3out <- map(A3xt[, 'value'], check_value)$value

  if (length(A3out) > 0) {
    A3ext_final <- A3xt[-A3out,]
  } else{
    A3ext_final <- A3xt
  }

  return(A3ext_final)

}
