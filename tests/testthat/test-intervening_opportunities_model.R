test_that("intervening_opportunities_model() works", {
  coefficients <- c(0.3, 0.4)
  tolerance <- 1e-2

  data <- get_data_mobility_reg()

  parameters <- 0.2
  for (diagonal in c(FALSE, TRUE)) {
    model <- intervening_opportunities_model(diagonal = diagonal,
                                             type = "exponential",
                                             parameters = parameters)

    test_fit_mobility_reg(diagonal = diagonal,
                          data = data,
                          model = model,
                          parameters = parameters,
                          coefficients = coefficients,
                          tolerance =  tolerance)
  }

  for (type in c("power", "radiation")) {
    parameters <- switch(
      type,
      power = 0.2,
      radiation = double()
    )
    model <- intervening_opportunities_model(diagonal = FALSE,
                                             type = type,
                                             parameters = parameters)
    test_fit_mobility_reg(diagonal = FALSE,
                          data = data,
                          model = model,
                          parameters = parameters,
                          coefficients = coefficients,
                          tolerance =  tolerance)
  }
})
