#' Template to bluid an ode system
#'
#' \code{upca_model} returns the simObj.
#' 
#' This a template from the simecol package. It produces guidelines to easily
#' compute odes.
#'
#' @param ... Nothing 
#' @return An ecological model of the class simObj.
#'
#'   This example is coming from the documentation of the simecol package. See
#'   \url{http://simecol.r-forge.r-project.org/} for more details.
#' @examples
#' sum(1:10)
#' sum(1:5, 6:10)
#' sum(F, F, F, T, T)
#'
#' sum(.Machine$integer.max, 1L)
#' sum(.Machine$integer.max, 1)
#'
#' \dontrun{
#' sum("a")
#' }
#'
#' @export

upca_model <- function() {
  new("odeModel",
    main = upca_ode,
    equations = list(
      f1 = function(x, y, k){x * y}, # Lotka-Volterra
      f2 = function(x, y, k){f1(x, y, k) / (1 + k * x)} # Holling II
      ),
    times = c(from = 0, to = 300, by = 0.1),
    parms = c(a = 1, b = 1, c = 10, alpha1 = 0.2, alpha2 = 1,
      k1 = 0.05, k2 = 0, wstar = 0.1),
    init = c(u = 10, v = 5, w = 0.1),
    solver = "lsoda"
    )
}

cellular_automata_model <- function() {
  new("gridModel",
    main = allelopathy_ca,
    times = c(from = 0, to = 200, by = 0.1),
    parms = list(b_s = 4, m_s = 1, m_p = 1, z = 4, gamma1 = 2, c1 = 0.5),
    init = matrix(sample.int(3, size = 20*20, replace = TRUE, prob = c(.1, .8, .1)), ncol = 20),
    solver = "myiteration_allelopathy"
    )
}
