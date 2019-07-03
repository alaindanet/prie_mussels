#' Check integration problem in runs 
#' 
#' Check if there is NaN (Not a Number) or negative densities during the run
#' 
#' @param df a dataframe containing the run of an ODE model
#' @return logical  
#'
#' @export
is_run_normal <- function (df) {
  check <- ifelse(
    any(sapply(df, simplify = "matrix", is.nan) | df < 0), FALSE, TRUE
  )
  return(check)
}
