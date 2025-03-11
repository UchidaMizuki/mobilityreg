test_that("exploring_opportunities_model_2() works", {
  coefficients_relevance <- 2
  coefficients_deterrence <- -2
  tolerance <- 1e-3

  data <- get_data_mobility_reg()

  for (diagonal in c(FALSE, TRUE)) {
    for (exploration_type in c("distance", "opportunity")) {
      for (deterrence_type in c("exponential", "power_law")) {
        cli::cli_inform("diagonal: {diagonal}, exploration_type: {exploration_type}, deterrence_type: {deterrence_type}")
        model <- exploring_opportunities_model_2(diagonal = diagonal,
                                                 exploration_type = exploration_type,
                                                 deterrence_type = deterrence_type)

        test_fit_mobility_reg_2(diagonal = diagonal,
                                data = data,
                                model = model,
                                coefficients_relevance = coefficients_relevance,
                                coefficients_deterrence = coefficients_deterrence,
                                tolerance = tolerance)
      }
    }
  }
})
