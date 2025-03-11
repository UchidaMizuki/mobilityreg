test_that("intervening_opportunities_model_2() works", {
  coefficients_relevance <- 2
  coefficients_deterrence <- -2
  tolerance <- 1e-3

  data <- get_data_mobility_reg()

  for (diagonal in c(FALSE, TRUE)) {
    cli::cli_inform("diagonal: {diagonal}, deterrence_type: exponential")
    model <- intervening_opportunities_model_2(diagonal = diagonal,
                                               deterrence_type = "exponential")

    test_fit_mobility_reg_2(diagonal = diagonal,
                            data = data,
                            model = model,
                            coefficients_relevance = coefficients_relevance,
                            coefficients_deterrence = coefficients_deterrence,
                            tolerance = tolerance)
  }

  for (deterrence_type in c("power_law", "radiation")) {
    cli::cli_inform("diagonal: FALSE, deterrence_type: {deterrence_type}")
    model <- intervening_opportunities_model_2(diagonal = FALSE,
                                               deterrence_type = deterrence_type)
    test_fit_mobility_reg_2(diagonal = diagonal,
                            data = data,
                            model = model,
                            coefficients_relevance = coefficients_relevance,
                            coefficients_deterrence = coefficients_deterrence,
                            tolerance = tolerance)
  }
})
