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



#' AHEI2010 Class
#'
#' A S4 class to store the scores from the AHEI-2010 calculation
#'
#' @slot rawValues
#' @slot componentScores
#' @slot AHEIScore
#' @export


setClass('AHEI',
         slots = list(
           rawValues = 'tbl_df',
           componentScores = 'tbl_df',
           AHEIScore = 'numeric'
         ))
