#' FFQ Class
#'
#' A S4 class to store Food Frequency Questionnaires (FFQs)
#'
#' @slot FFQ a list of cleaned FFQ tibbles
#' @slot Nutrients a list of tibbles containing daily nutrient intake values for each FFQ food component
#' @export
#'

setClass('ffqr',
         slots = list (FFQ = 'list',
                       Nutrients = 'list'))
