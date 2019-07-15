#' AHEI-2010
#' @rdname AHEI2010
#'
#' @param object a `ffqr` object
#' @return a list of `tibbles`
#' @export

setGeneric(
  name = 'AHEI2010',
  def = function(object)
  {
    standardGeneric('AHEI2010')
  }
)

#' Calculate Nutrients
#' @rdname calculate_nutrients
#'
#' @param object a `ffqr` object
#' @return a `ffqr` object
#' @export

setGeneric(
  name = 'calculate_nutrients',
  def = function(object)
  {
    standardGeneric('calculate_nutrients')
  }
)

#' Total Nutrients
#' @rdname total_nutrients
#'
#' @param object a `ffqr` object
#' @return a list of `tibbles`
#' @export

setGeneric(
  name = 'total_nutrients',
  def = function(object)
  {
    standardGeneric('total_nutrients')
  }
)


#' AHEI2010 Alcohol Correction
#' @rdname AlcoholAHEI
#'
#' @param object a `AHEI` object
#' @param gender a character string of Gender (F or M)
#' @return a `AHEI` object
#' @export

setGeneric(
  name = 'AlcoholAHEI',
  def = function(object, gender)
  {
    standardGeneric('AlcoholAHEI')
  }
)






