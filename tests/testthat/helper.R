get_mobility_data <- function(diagonal,
                              seed = 1234,
                              location = letters[1:5]) {
  set.seed(seed)

  data <- tidyr::expand_grid(origin = location,
                             destination = location)
  data$distance <- runif(nrow(data))
  data$x1 <- runif(nrow(data))
  data$x2 <- runif(nrow(data))

  if (!diagonal) {
    data <- dplyr::filter(data, origin != destination)
  }
  data
}

test_fit_mobility_reg <- function(diagonal, model, parameters, coefficients,
                                  tolerance = 1e-3) {
  data <- get_mobility_data(diagonal = diagonal)
  model <- model(diagonal = diagonal,
                 parameters = parameters)
  reg <- fit_mobility_reg(data = data,
                          model = model,
                          parameters = parameters,
                          coefficients = coefficients)

  expect_equal(reg@coefficients, coefficients,
               tolerance = tolerance)
  expect_equal(reg@model@parameters, parameters,
               tolerance = tolerance)

  predicted <- predict(reg,
                       new_data = data,
                       class = "dibble")
  expect_true(all(dplyr::near(dibble::apply(predicted, "origin", sum), 1, tol = tolerance)))

  if (!diagonal) {
    expect_true(all(dibble::diag(predicted, "origin") == 0))
  }
}

fit_mobility_reg <- function(data, model, parameters, coefficients,
                             formula = y ~ x1 + x2) {
  reg <- mobility_reg(model = model,
                      formula = formula,
                      coefficients = coefficients)
  data$y <- predict(reg,
                    new_data = data)

  reg@coefficients <- vctrs::vec_init_along(reg@coefficients)
  reg@model@parameters <- vctrs::vec_init_along(reg@model@parameters)
  fit(reg, data)
}
