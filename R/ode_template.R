#' ODE definitions  
#' 
#' Describe an ode system
#'
#' @param time c(from = 0, to = 100, by = 0.1)
#' @param init vector of initial values for variables.
#' @return a list of derivative values.
#'
#' @export
upca_ode <- function(time, init, parms) {
  u <- init["u"]
  v <- init["v"]
  w <- init["w"]
  f <- function(x, y, k){x * y}
  with(as.list(parms), {
    du <- a * u - alpha1 * f(u, v, k1)
    dv <- -b * v + alpha1 * f(u, v, k1) - alpha2 * f(v, w, k2)
    dw <- -c * (w - wstar) + alpha2 * f(v, w, k2)

    list(c(du, dv, dw))
})
}

#' Solver to stop at steady state 
#' 
#'  
#'
#' @param time c(from = 0, to = 100, by = 0.1)
#' @param init vector of initial values for variables.
#' @return a list of derivative values.
#'
#' @export
steady_state_upca <- function(time, init, func, parms) {
  root <- function(time, init, parms) {
    dstate <- unlist(upca_ode(time, init, parms))
    return(sum(abs(dstate)) - 1e-4)
  }
  lsodar(time, init, func, parms, rootfun = root)
}
