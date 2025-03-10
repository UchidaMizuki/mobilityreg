#' @export
mobility_reg_2 <- function(model, formula_relevance, formula_deterrence, data, ...) {
  mobility_reg_2 <- MobilityReg_2(model = model,
                                  formula_relevance = formula_relevance,
                                  formula_deterrence = formula_deterrence)
  fit(mobility_reg_2, data, ...)
}

MobilityReg_2 <- S7::new_class(
  "MobilityReg_2",
  properties = list(
    coefficients_relevance = S7::class_numeric,
    coefficients_deterrence = S7::class_numeric,
    formula_relevance = S7::class_formula,
    formula_deterrence = S7::class_formula,
    model = MobilityModel_2,
    optimised = S7::class_list
  )
)

S7::method(predict, MobilityReg_2) <- function(object, new_data,
                                               class = c("vector", "list")) {
  new_data <- MobilityReg_2_new_data(object, new_data)
  predict(object@model, new_data$data,
          class = class)
}

S7::method(fit, MobilityReg_2) <- function(object, data,
                                             parameters_init = NULL,
                                             ...) {
  # Ignore the intercept of the relevance model
  n_coefficients_relevance <- MobilityReg_2_n_coefficients(object@formula_relevance, data) - 1
  n_coefficients_deterrence <- MobilityReg_2_n_coefficients(object@formula_deterrence, data)

  parameters <- vctrs::vec_recycle(parameters_init %||% 0, n_coefficients_relevance + n_coefficients_deterrence)
  update_parameters <- function(object, parameters) {
    parameters <- vctrs::vec_chop(parameters,
                                  sizes = c(n_coefficients_relevance, n_coefficients_deterrence))
    object@coefficients_relevance <- parameters[[1]]
    object@coefficients_deterrence <- parameters[[2]]
    object
  }
  fn <- function(parameters) {
    object <- update_parameters(object, parameters)

    new_data <- MobilityReg_2_new_data(object, data)
    probability <- predict(object@model, new_data$data,
                           class = "list")

    probability <- vctrs::list_unchop(probability)

    new_data <- vctrs::list_unchop(new_data$data)
    flow <- vctrs::vec_slice(data$flow, new_data$id)

    log_likelihood <- dibble::ifelse(flow == 0, 0, flow * log(probability))
    -sum(log_likelihood)
  }
  optimised <- optim(parameters, fn, ...)

  object@optimised <- optimised
  update_parameters(object, optimised$par)
}

MobilityReg_2_new_data <- function(object, new_data) {
  # relevance
  formula_relevance <- object@formula_relevance

  model_frame_relevance <- stats::model.frame(formula_relevance, new_data)
  model_matrix_relevance <- stats::model.matrix(formula_relevance, model_frame_relevance)
  model_offset_relevance <- stats::model.offset(model_frame_relevance) %||% 0

  relevance <- exp(as.vector(model_matrix_relevance[, -1, drop = FALSE] %*% object@coefficients_relevance) + model_offset_relevance)

  # deterrence
  formula_deterrence <- object@formula_deterrence

  model_frame_deterrence <- stats::model.frame(formula_deterrence, new_data)
  model_matrix_deterrence <- stats::model.matrix(formula_deterrence, model_frame_deterrence)
  model_offset_deterrence <- stats::model.offset(model_frame_deterrence) %||% 0

  deterrence <- exp(as.vector(model_matrix_deterrence %*% object@coefficients_deterrence) + model_offset_deterrence)

  new_data <- new_data[c("origin", "destination", "distance")]
  new_data$relevance <- relevance
  new_data$deterrence <- deterrence

  new_data <- tibble::rowid_to_column(new_data, "id")
  new_data <- dplyr::arrange(new_data,
                             dplyr::desc(.data$origin == .data$destination),
                             .data$distance)
  tidyr::nest(new_data,
              .by = "origin")
}

MobilityReg_2_n_coefficients <- function(formula, data) {
  model_frame <- stats::model.frame(formula, data)
  model_matrix <- stats::model.matrix(formula, model_frame)
  ncol(model_matrix)
}
