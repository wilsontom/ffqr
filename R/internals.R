#' Check Item Value
#'
#' Check that each extracted variable is a food consumption catagoryu
#'
#' @param x the `value` column of a FFQ `tibble`
#' @return a numeric vector of position for removal
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
  v09 <- '6\\+ per day'

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


#' Convert to Servings
#'
#' Convert FFQ responses to approximate servings per day
#'
#' @param x a FFQ response value
#' @return a numeric value for the approimate servings per day
#' @keywords internal

convert_servings <- function(x)
{
  if (x == 'Never or less than once/ month') {
    serving <- 0
  }
  if (x == '1-3 per month') {
    serving <- 0.07
  }
  if (x == 'Once a week') {
    serving <- 0.14
  }
  if (x == '2-4 per week') {
    serving <- 0.43
  }
  if (x == '5-6 per week') {
    serving <- 0.79
  }
  if (x == 'Once a day') {
    serving <- 1
  }
  if (x == '2-3 per day') {
    serving <- 2.5
  }
  if (x == '4-5 per day') {
    serving <- 4.5
  }
  if (x == '6\\+ per day') {
    serving <- 6
  }
  if(x == '6+ per day') {
    serving <- 6
  }
  return(serving)

}
