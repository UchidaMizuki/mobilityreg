#' @export
mobility_reg <- function(model, formula_relevance, formula_deterrence, data) {
  mobility_reg <- MobilityReg(model = model,
                              formula_relevance = formula_relevance,
                              formula_deterrence = formula_deterrence)
  fit(mobility_reg, data)
}

MobilityReg <- S7::new_class(
  "MobilityReg",
  properties = list(
    coefficients_relevance = S7::class_numeric,
    coefficients_deterrence = S7::class_numeric,
    formula_relevance = S7::class_formula,
    formula_deterrence = S7::class_formula,
    model = MobilityModel
  )
)

S7::method(predict, MobilityReg) <- function(object, new_data,
                                             class = c("vector", "dibble")) {
  new_data <- MobilityReg_new_data(object, new_data)
  predict(object@model, new_data,
          class = class)
}

#' @include mobilityreg-package.R
S7::method(fit, MobilityReg) <- function(object, data, ...) {
  # Ignore the intercept of the relevance model
  n_coefficients_relevance <- MobilityReg_n_coefficients(object@formula_relevance, data) - 1
  n_coefficients_deterrence <- MobilityReg_n_coefficients(object@formula_deterrence, data)

  par <- vctrs::vec_rep(0, n_coefficients_relevance + n_coefficients_deterrence)
  update_par <- function(object, par) {
    par <- vctrs::vec_chop(par,
                           sizes = c(n_coefficients_relevance, n_coefficients_deterrence))
    object@coefficients_relevance <- par[[1]]
    object@coefficients_deterrence <- par[[2]]
    object
  }
  fn <- function(par) {
    object <- update_par(object, par)
    probability <- predict(object, data,
                           class = "vector")

    if (!object@model@diagonal) {
      flow_diagonal <- dplyr::filter(data, .data$origin == .data$destination)
      flow_diagonal <- flow_diagonal$flow

      if (!all(flow_diagonal == 0)) {
        cli::cli_abort("If {.code model@diagonal} is {.code FALSE}, the internal flow must be zero.")
      }
    }
    flow <- data$flow

    log_likelihood <- dibble::ifelse(flow == 0, 0, flow * log(probability))
    -sum(log_likelihood)
  }
  optimised <- optim(par, fn, ...)
  update_par(object, optimised$par)
}

MobilityReg_new_data <- function(object, new_data) {
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
  new_data
}

MobilityReg_n_coefficients <- function(formula, data) {
  model_frame <- stats::model.frame(formula, data)
  model_matrix <- stats::model.matrix(formula, model_frame)
  ncol(model_matrix)
}
