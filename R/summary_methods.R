#' Extract the average density of the last timesteps 
#'
#' This function extracts the density of the last timesteps and average it. 
#' 
#' @param run a dataframe. It contains the variable time and the density of the
#' of the state variable.
#' @param cut_row a integer. Define at how many timesteps from the last one should be
#' selected to average density 
#' @return a dataframe.
#' @export
avg_runs <- function(x, ...) UseMethod("avg_runs")
avg_runs.default <- function(x) "Unknown class"
avg_runs.data.frame <- function(run, cut_row = 10) {

  if (nrow(run) < cut_row) {
    warnings("cut_row is ", cut_row, "  and the number of row of the data.frame
      is ", nrow(run), ". cut_row has been set to ", nrow(run), ".")
    cut_row <- nrow(run)
  }

  out <- run %>%
    dplyr::slice( (n() - cut_row) : n()) %>% # Keep the last simulation
    tidyr::gather(species, rho, -time) %>%
    dplyr::group_by(species) %>%
    dplyr::summarise(
      rho = mean(rho, na.rm = TRUE),
      time = last(time)
      ) %>%
    dplyr::ungroup() %>%
    tidyr::spread(species, rho)

  # check negative or NaN
  out$status <- is_run_normal(run)

  return(out)
}
