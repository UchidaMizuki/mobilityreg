test_that("gravity_model() works", {
  coefficients_relevance <- 1
  coefficients_deterrence <- -1
  tolerance <- 1e-3

  data <- get_data_mobility_reg(distance_diagonal = 1e-2)

  for (diagonal in c(FALSE, TRUE)) {
    for (type in c("exponential", "power")) {
      cli::cli_inform("diagonal: {diagonal}, type: {type}")
      model <- gravity_model(diagonal = diagonal,
                             type = type)
      test_fit_mobility_reg(diagonal = diagonal,
                            data = data,
                            model = model,
                            coefficients_relevance = coefficients_relevance,
                            coefficients_deterrence = coefficients_deterrence,
                            tolerance = tolerance)
    }
  }
})
