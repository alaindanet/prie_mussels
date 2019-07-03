
#' Run simulation over different initial scenarii
#'
#' The function run simulations of parameters values. It takes list of parameters values and initial values. 
#'
#' @inheritParams run_2d_param_comb
#' @param scenarii a list of vector. A vector contains initial values of the
#' model  
#'
#' @export
run_scenarii <- function (
  param_comb  = NULL,
  scenarii    = NULL,
  eq_comb     = NULL,
  model       = NULL,
  time        = NULL,
  param       = NULL,
  expand = TRUE,
  nb_cores    = NULL,
  solver_type = NULL,
  set_tail    = NULL,
  nrep        = NULL) {

  if (is.null(param_comb)) {
    message("You did not provide list of parameters.\n
      Running with parameter specification.")
  }
  if (is.null(model)) {
   stop("Please specify a model")
  }

  # Define parameters:
  if (!is.null(time)) {
  simecol::times(model) <- time
  }
  if (!is.null(param)) {
    simecol::parms(model)[names(param)] <- param
  }
  # Define the solver type: see simecol::solver()
  if (!is.null(solver_type)) {
    simecol::solver(model) <- solver_type
  }
  if (is.null(scenarii)) {
    scenarii  <- list(default = simecol::init(model))
  }
  # Define the combination of parameters:
  if (!is.null(nrep)) {
  param_comb$rep <- seq.int(1, nrep)
  }
  scenar_param_comb <- param_comb
  scenar_param_comb$scenario <- names(scenarii)

  # Define equations if they are defined:
  if (!is.null(simecol::equations(model))) {
    if (!is.null(eq_comb)) {
      dfs <- lapply(eq_comb, function(x) names(x))
      scenar_param_comb <- cbind(scenar_param_comb, dplyr::bind_rows(dfs))
    }
  }

  if (expand) {
    comb <- expand.grid(scenar_param_comb)
  } else {
   comb %<>% tibble::as_tibble()
  }
  comb %<>%
    dplyr::mutate(# extract init values:
      inits = purrr::map(scenario, function(x) scenarii[[x]])
    )
  # extract equations:
  ## Trick to return a data.frame even if 1 function:
  eq_temp_df <- as.data.frame(comb[, colnames(comb) %in% names(eq_comb)])
  colnames(eq_temp_df) <- colnames(comb)[colnames(comb) %in% names(eq_comb)]
  comb[["equations"]] <- get_equation_sim(
    eq_df = eq_temp_df,
    eq_list = eq_comb
    ) 
    
    param_combination <- dplyr::select(comb, -scenario, -inits) %>%
    df2list()

  # Run the simulations
  run <- parallel::mcMap(
    run_simecol,
    inits = comb[["inits"]],
    param = param_combination,
    eq = comb[["equations"]],
    MoreArgs = list(
      model = model,
      set_tail = set_tail
    )
  )
  # Prepare output:
  output <- tibble::as.tibble(comb) %>%
    dplyr::mutate(
      scenario = comb[, "scenario"],
      run = run
      ) %>%
  dplyr::select(-inits) %>%
  dplyr::select(scenario, dplyr::everything()) %>%
  dplyr::select(-run, dplyr::everything())

  # Save model parameters:
  ## Parallel parameters:
  parallel_param <- which(!names(simecol::parms(model)) %in% names(param_comb))
  basis_param <- simecol::parms(model)[parallel_param]
  if (!is.null(param)) {
    basis_param[names(param)] <- param
  }

  return(
    structure(
      list(
	model      = model,
	inits      = scenarii,
	param      = basis_param,
	param_comb = param_comb,
	run        = output
	),
    class = c("scenarii", "list"))
    )
}


#' Run the model by specifying initial values, parameters and the model  
#' 
#' Run the model over multidimensional param_comb and initial values.
#' 
#' @param inits a vector of initial values. See simecole::init 
#' @param params a named vector of length one
#' @param model a function containing a odeModel
#' @param set_tail integer. Keep only the n last time steps of a simulations
#'
#' @return a data.frame 
#' @export
run_simecol <- function(inits = NULL, params = NULL, model = NULL,
  eq = NULL, set_tail = NULL) {

  simecol::parms(model)[names(params)] <- params
  simecol::init(model) <- inits

  run <- simecol::sim(model)

  # For big simulations, keep only the last step
  if (!is.null(set_tail)) {
    output <- simecol::out(run) %>%
      tail(., set_tail)
  } else {
    output <- simecol::out(run)
  }
  return(output)
}

#' Transform a equation setup to list of function 
#'
#' @param eq_df a data.frame. Names of columns are the function names. Rows
#' contains the names of the equation specification and correspond to equation
#' specification for a given simulation.
#' @param eq_list a list
#' @return a list. An element of the list indicates the functions specification
#' for a given simulation.
#'
#' @export
get_equation_sim <- function(eq_df, eq_list) {
  output <- apply(eq_df, 1, FUN = function(x) {
    fun <- vector("list", length(x))
    for (y in seq_along(x)) {
      fun[[y]] <- unlist(eq_list[[y]][[x[y]]])
    }
    names(fun) <- names(eq_list)
    fun
})
  output
}

#' Convert a data.frame to a named list
#'
#' @param x a data.frame 
#' @return a list. 
#' @details Each element of the list contains a named vector. The names
#' are those of the columns of the input.
#'
#' @export
df2list <- function (x) {
  new_list <- apply(x, 1, as.list)
  lapply(new_list, unlist)
}

