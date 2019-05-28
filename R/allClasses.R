#' FFQ Class
#'
#' @slot FFQ
#' @slot Portions
#' @slot Nutrients
#' @slot Components
#'


setClass(
  'FFQ',
  slots = list (
    FFQ = 'tbl_df',
    Portions = 'tbl_df',
    Nutrients = 'tbl_df',
    Components = 'tbl_df'

  )
)


