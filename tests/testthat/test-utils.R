test_that("apply_by_distance() works", {
  reg <- mobility_reg(model = get_empty_mobility_model(),
                      formula = y ~ x1 + x2,
                      coefficients = c(3, 4))
  data <- get_data_mobility_reg()
  data <- get_data_mobity_model(reg, data)

  x <- data$x
  distance <- data$distance

  x_min <- apply_by_distance(x, distance, min)
  x_subtract_max <- apply_by_distance(-x, distance, max)
  expect_equal(x_min, -x_subtract_max)

  x_sum <- apply_by_distance(x, distance, sum)
  x_cumsum <- apply_by_distance(x, distance, cumsum)
  x_cumsum_last <- apply_by_distance(x_cumsum, distance, dplyr::last)
  expect_equal(x_sum, x_cumsum_last)
})
