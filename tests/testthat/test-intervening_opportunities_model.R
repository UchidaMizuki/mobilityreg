test_that("intervening_opportunities_model() works", {
  coefficients_relevance <- 1
  coefficients_deterrence <- -1
  tolerance <- 1e-3

  data <- get_data_mobility_reg()

  for (diagonal in c(FALSE, TRUE)) {
    cli::cli_inform("diagonal: {diagonal}, type: exponential")
    model <- intervening_opportunities_model(diagonal = diagonal,
                                             type = "exponential")

    test_fit_mobility_reg(diagonal = diagonal,
                          data = data,
                          model = model,
                          coefficients_relevance = coefficients_relevance,
                          coefficients_deterrence = coefficients_deterrence,
                          tolerance = tolerance)
  }

  for (type in c("power", "radiation")) {
    cli::cli_inform("diagonal: FALSE, type: {type}")
    model <- intervening_opportunities_model(diagonal = FALSE,
                                             type = type)
    test_fit_mobility_reg(diagonal = diagonal,
                          data = data,
                          model = model,
                          coefficients_relevance = coefficients_relevance,
                          coefficients_deterrence = coefficients_deterrence,
                          tolerance = tolerance)
  }
})
