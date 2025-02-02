get_data_mobility_reg <- function(distance_diagonal = 0,
                                  seed = 1234,
                                  location = letters[1:5]) {
  set.seed(seed)

  data <- tidyr::expand_grid(origin = location,
                             destination = location)
  data <- dplyr::mutate(data,
                        distance = dplyr::if_else(.data$origin == .data$destination,
                                                  distance_diagonal,
                                                  runif(dplyr::n())),
                        x1 = runif(dplyr::n()),
                        x2 = runif(dplyr::n()))
  data
}

test_fit_mobility_reg <- function(diagonal, data, model, parameters, coefficients,
                                  tolerance = 1e-3) {
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

fit_mobility_reg <- function(model, data, parameters, coefficients,
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
