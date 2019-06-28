#' Assign Incremental Scores
#'
#' Create incremental scores for each
#'
#' @param start a numeric value where the scoring starts
#' @param end a numeric value where the scoring ends
#' @param n the number of groups that the increments are divided into
#' @return a `tibble` of incremental group thresholds
#'
#' @keywords internal

score_increments <- function(start, end, n)
{
  seql <- seq(from = start, to = end, by = 0.0001)

  seq_cuts <- cut(seql, breaks = n, right = TRUE)

  seqcut_unique <- unique(seq_cuts)

  seqcut_unique <- str_remove(seqcut_unique, ']')
  seqcut_unique <- str_remove(seqcut_unique, '\\(')

  lowint <- str_split(seqcut_unique, ',') %>% map_chr(., ~ {
    .[[1]]
  })

  highint <- str_split(seqcut_unique, ',') %>% map_chr(., ~ {
    .[[2]]
  })

  increment_index <-
    tibble::tibble(
      Score = 1:n,
      Lower = as.numeric(lowint),
      Upper = as.numeric(highint)
    )

  return(increment_index)
}
