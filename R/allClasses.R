#' FFQ Class
#'
#' @slot FFQ
#' @slot Nutrients
#' @export
#'

setClass('ffqr',
         slots = list (FFQ = 'list',
                       Nutrients = 'list'))
