
allelopathy_ca <- function(time, init, parms) {

  landscape <- init
  rho_producer <- sum(landscape == 1) / length(landscape)
  rho_sensitive <- sum(landscape == 2) / length(landscape)
  parms$b_p <- parms$b_s - parms$c1

  # 4 Neighbors: 
  neigh4 <- matrix(rep(0, 9), nrow = 3)
  neigh4[c(2, 4, 6, 8)] <- 1
  ## Producers in neighborhood:
  neigh_p <- simecol::neighbors(landscape, state = 1,  wdist = neigh4, bounds = 1)
  ## Sensitive strains in neighborhood:
  neigh_s <- simecol::neighbors(landscape, state = 2,  wdist = neigh4, bounds = 1)
  ## Empty cells in neighborhood:
  neigh_e <- simecol::neighbors(landscape, state = 3,  wdist = neigh4, bounds = 1)

  with(parms, {
    # growth producer

    # Rules
    colonization_producer <- (b_p * neigh_e / z) * DELTAT
    colonization_sensitive <- (b_s * neigh_e / z) * DELTAT
    death_producer <- (m_p) * DELTAT
    death_sensitive <- (m_s + gamma1 * neigh_p) * DELTAT

    # Apply rules
    rnum <- runif(length(landscape)) # one random number between 0 and 1 for each cell
    new_landscape <- landscape

    ## New producers 
    new_landscape[which(landscape == 3 & rnum <= colonization_producer)] <- 1

    ## New sensitive 
    new_landscape[which(landscape == 3 & rnum > colonization_producer &
      rnum <= colonization_producer + colonization_sensitive)] <- 2

    ## New empty
    new_landscape[which(landscape == 1 & rnum <= death_producer)] <- 3
    new_landscape[which(landscape == 2 & rnum <= death_sensitive)] <- 3

    # check for sum of probabilities to be inferior 1 and superior 0
    if (any(c(colonization_producer + colonization_sensitive,
	  colonization_sensitive, colonization_producer) > 1 )) { warning("a set
      probability is exceeding 1 in run! decrease delta!!!")
    }
    if (any(c(colonization_producer, colonization_sensitive,
	  death_producer, death_sensitive) < 0)) {
      warning("a set probability falls below 0 in run balance parameters!!!")
    }

  return(new_landscape)

})
}

myiteration_allelopathy <- function(y, times=NULL, func=NULL, parms=NULL,
  animate=FALSE, ...) {
  observer <- function(landscape) {
    # density of cells 
    rho_producer <- sum(landscape == 1) / length(landscape)
    rho_sensitive <- sum(landscape == 2) / length(landscape)
    rho_empty <- sum(landscape == 3) / length(landscape)

    c(
      producer = rho_producer,
      sensitive = rho_sensitive,
      empty = rho_empty
      )
  }
  init <- y@init
  times <- fromtoby(y@times)
  func <- y@main
  parms <- y@parms
  inputs <- y@inputs
  equations <- y@equations
  equations <- addtoenv(equations)
  environment(func) <- environment()
  parms$DELTAT <- 0
  res <- observer(init)
  out <- res
  for (i in 2:length(times)) {
    time <- times[i]
    parms$DELTAT <- times[i] - times[i-1]
    init <- func(time, init, parms)
    res <- observer(init)
    out <- rbind(out, res)
  }
  row.names(out) <- NULL
  out <- cbind(times, out)
  as.data.frame(out)
}
