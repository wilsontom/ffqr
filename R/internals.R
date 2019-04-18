#' Check Item Value
#'
#' Check that each extracted variable is a food consumption catagoryu
#'
#' @param
#' @return
#' @keywords internal

check_value <- function(x)
{
  v01 <- 'Never or less than once/ month'
  v02 <- '1-3 per month'
  v03 <- 'Once a week'
  v04 <- '2-4 per week'
  v05 <- '5-6 per week'
  v06 <- 'Once a day'
  v07 <- '2-3 per day'
  v08 <- '4-5 per day'
  v09 <- '6+ per day'

  pattern_values <-
    paste0(v01,
           '|',
           v02,
           '|',
           v03,
           '|',
           v04,
           '|',
           v05,
           '|',
           v06,
           '|',
           v07,
           '|',
           v08,
           '|',
           v09)

  value_out <- which(grepl(pattern_values, x) == FALSE)

  return(value_out)

}
