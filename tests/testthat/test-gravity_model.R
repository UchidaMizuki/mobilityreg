test_that("gravity_model() works", {
  parameters <- 2
  coefficients <- c(3, 4)
  tolerance <- 1e-3

  data <- get_data_mobility_reg(distance_diagonal = 1e-2)

  for (diagonal in c(FALSE, TRUE)) {
    for (type in c("exponential", "power")) {
      model <- gravity_model(diagonal = diagonal,
                             type = type,
                             parameters = parameters)
      test_fit_mobility_reg(diagonal = diagonal,
                            data = data,
                            model = model,
                            parameters = parameters,
                            coefficients = coefficients,
                            tolerance = tolerance)
    }
  }
})
