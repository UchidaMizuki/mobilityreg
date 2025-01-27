test_that("gravity_model() works", {
  parameters <- 2
  coefficients <- c(3, 4)

  tolerance <- 1e-3
  test_fit_mobility_reg(diagonal = FALSE,
                        model = gravity_model,
                        parameters = parameters,
                        coefficients = coefficients,
                        tolerance = tolerance)
  test_fit_mobility_reg(diagonal = TRUE,
                        model = gravity_model,
                        parameters = parameters,
                        coefficients = coefficients,
                        tolerance = tolerance)
})
