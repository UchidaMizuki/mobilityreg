test_that("apply_by_distance() works", {
  reg <- MobilityReg(model = get_empty_mobility_model(),
                     formula_relevance = ~ x_relevance,
                     formula_deterrence = ~ 1,
                     coefficients_relevance = 1,
                     coefficients_deterrence = -1)
  data <- get_data_mobility_reg()
  data <- get_data_mobity_model(reg, data)

  relevance <- data$relevance
  distance <- data$distance

  relevance_min <- apply_by_distance(relevance, distance, min)
  relevance_subtract_max <- apply_by_distance(-relevance, distance, max)
  expect_equal(relevance_min, -relevance_subtract_max)

  relevance_sum <- apply_by_distance(relevance, distance, sum)
  relevance_cumsum <- apply_by_distance(relevance, distance, cumsum)
  relevance_cumsum_last <- apply_by_distance(relevance_cumsum, distance, dplyr::last)
  expect_equal(relevance_sum, relevance_cumsum_last)
})
