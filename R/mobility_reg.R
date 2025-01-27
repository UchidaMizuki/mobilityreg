mobility_reg <- function(model, formula,
                         data = NULL,
                         coefficients = double()) {
  mobility_reg <- MobilityReg(coefficients = coefficients,
                              formula = formula,
                              model = model)

  if (!is.null(data)) {
    mobility_reg <- fit(mobility_reg, data)
  }
  mobility_reg
}

MobilityReg <- S7::new_class(
  "MobilityReg",
  properties = list(
    coefficients = S7::class_numeric,
    formula = S7::class_formula,
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
  n_coefficients <- MobilityReg_n_coefficients(object, data)
  n_parameters <- object@model@n_parameters

  par <- vctrs::vec_rep(0, n_coefficients + n_parameters)
  update_par <- function(object, par) {
    object@coefficients <- par[1:n_coefficients]
    object@model@parameters <- par[(n_coefficients + 1):(n_coefficients + n_parameters)]
    object
  }
  fn <- function(par) {
    object <- update_par(object, par)
    probability <- predict(object, data,
                           class = "vector")

    lhs <- rlang::f_lhs(object@formula)
    if (!object@model@diagonal) {
      y_diagonal <- dplyr::filter(data, .data$origin == .data$destination)
      y_diagonal <- y_diagonal[[lhs]]

      if (!all(y_diagonal == 0)) {
        cli::cli_abort("If {.code model@diagonal} is {.code FALSE}, the internal flow must be zero.")
      }
    }
    y <- data[[lhs]]

    log_likelihood <- dibble::ifelse(y == 0, 0, y * log(probability))
    -sum(log_likelihood)
  }
  optimised <- optim(par, fn, ...)
  update_par(object, optimised$par)
}

MobilityReg_new_data <- function(object, new_data) {
  formula <- object@formula
  rlang::f_lhs(formula) <- NULL

  model_frame <- stats::model.frame(formula, new_data)
  model_matrix <- stats::model.matrix(formula, model_frame)
  model_offset <- stats::model.offset(model_frame) %||% 0

  x <- exp(as.vector(model_matrix[, -1] %*% object@coefficients) + model_offset)
  y <- dplyr::select(new_data, rlang::f_lhs(formula))

  new_data <- dplyr::select(new_data, "origin", "destination", "distance")
  tibble::add_column(new_data, x, y)
}

MobilityReg_n_coefficients <- function(object, data) {
  formula <- object@formula
  model_frame <- stats::model.frame(formula, data)
  model_matrix <- stats::model.matrix(formula, model_frame)
  ncol(model_matrix) - 1
}
