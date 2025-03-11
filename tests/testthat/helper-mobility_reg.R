get_data_mobility_reg <- function(distance_diagonal = 0,
                                  seed = 1234,
                                  location = letters[1:10]) {
  set.seed(seed)

  data <- tidyr::expand_grid(origin = location,
                             destination = location)
  data <- dplyr::mutate(data,
                        distance = dplyr::if_else(.data$origin == .data$destination,
                                                  distance_diagonal,
                                                  rlnorm(dplyr::n())),
                        x_relevance = rnorm(dplyr::n()))
  data
}

test_fit_mobility_reg <- function(diagonal,
                                  data,
                                  model,
                                  coefficients_relevance,
                                  coefficients_deterrence,
                                  tolerance,
                                  ...) {
  reg <- fit_mobility_reg(data = data,
                          model = model,
                          coefficients_relevance = coefficients_relevance,
                          coefficients_deterrence = coefficients_deterrence,
                          ...)
  data$probability <- predict(reg,
                              new_data = data)
  probability_sum <- dplyr::summarise(data,
                                      dplyr::across("probability", sum),
                                      .by = "origin")
  expect_true(all(dplyr::near(probability_sum$probability, 1, tol = tolerance)))

  if (!diagonal) {
    data_diagonal <- dplyr::filter(data, .data$origin == .data$destination)
    expect_true(all(data_diagonal$probability == 0))
  }

  expect_equal(reg@coefficients_relevance, coefficients_relevance,
               tolerance = tolerance)
  expect_equal(reg@coefficients_deterrence, coefficients_deterrence,
               tolerance = tolerance)
}

fit_mobility_reg <- function(data,
                             model,
                             coefficients_relevance,
                             coefficients_deterrence,
                             ...) {
  reg <- MobilityReg(coefficients_relevance = coefficients_relevance,
                     coefficients_deterrence = coefficients_deterrence,
                     formula_relevance = ~ x_relevance,
                     formula_deterrence = ~ 1,
                     model = model)
  data$flow <- predict(reg,
                       new_data = data)

  reg@coefficients_relevance <- vctrs::vec_init_along(reg@coefficients_relevance)
  reg@coefficients_deterrence <- vctrs::vec_init_along(reg@coefficients_deterrence)
  fit(reg, data, ...)
}
