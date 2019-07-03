context("run_scenarii")

library('simecol')
library('magrittr')
data(lv)

test_that("Basic works", {

  scenarii <- list(
    default = simecol::init(lv),
    high_prey = c(prey = 1, predator = 1)
  )
  param_combination <- expand.grid(
    k2 = c(.1, .2)
  )
  output <- run_scenarii(
    param_comb = param_combination,
    model = lv,
    time = NULL,
    scenarii = scenarii
  )
  expect_is(output$run, "data.frame")
  expect_is(output$model, "odeModel")
})

data(upca)
test_that("equations are correctly handle", {

  eq_spec <- list(
    growth = list(
      typeI = function (x, y, k) { x * y },
      typeII = function (x, y, k) { (x * y) / (1 + k * x) }
      ),
    death = list(
      basic = function (x, m) {x * m},
      complex = function (x, y) {(x * y) * m}
    )
  )
  dfs <- lapply(eq_spec, function(x) names(x))
  eq_df <- dplyr::bind_rows(dfs)

  output <- get_equation_sim(eq_df = eq_df, eq_list = eq_spec)
  expected <- list(
    list(
      growth = function (x, y, k) { x * y },
      death = function (x, m) {x * m}
      ),
    list(
      growth = function (x, y, k) { (x * y) / (1 + k * x) },
      death = function (x, y) {(x * y) * m}
    )
  )
  expect_equal(output, expected)

  holling <- list(
    f = list(
      typeI = function (x, y, k) { x * y },
      typeII = function (x, y, k) { (x * y) / (1 + k * x) }
      ))
  dfs <- lapply(holling, function(x) names(x))
  eq_df <- dplyr::bind_rows(dfs)

  output <- get_equation_sim(eq_df = eq_df, eq_list = holling)
  expected <- list(
    list(f = function (x, y, k) { x * y }),
    list(f = function (x, y, k) { (x * y) / (1 + k * x) })
    )
  expect_equal(output, expected)

})

test_that("With parallel equations", {
  holling <- list(
    f = list(
      typeI = function (x, y, k) { x * y },
      typeII = function (x, y, k) { (x * y) / (1 + k * x) }
      ))
  output <- run_scenarii(
    param_comb = NULL,
    eq_comb = holling,
    model = upca,
    time = NULL,
    scenarii = NULL
  )
})

