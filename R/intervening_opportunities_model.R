intervening_opportunities_model <- function(diagonal, type,
                                            parameters = switch(type,
                                                                exponential = NA_real_,
                                                                power = NA_real_,
                                                                radiation = double())) {
  type <- rlang::arg_match(type, c("exponential", "power", "radiation"))

  if (type %in% c("power", "radiation") && diagonal) {
    cli::cli_abort('If {.arg type} is {.code "power"} or {.code "radiation"}, {.arg diagonal} must be {.code FALSE}.')
  }

  probability <- function(object, data) {
    opportunity_cumsum <- apply_by_distance(data$x, data$distance, cumsum)
    opportunity_cumsum_lag <- apply_by_distance(opportunity_cumsum, data$distance, dplyr::lag,
                                                default = 0)
    opportunity_cumsum_last <- apply_by_distance(opportunity_cumsum, data$distance, dplyr::last)
    opportunity_diagonal <- dibble::diag(data$x,
                                         axes = "origin")

    upper_probability <- upper_probability(opportunity_cumsum = opportunity_cumsum,
                                           opportunity_diagonal = opportunity_diagonal,
                                           parameters = object@parameters,
                                           diagonal = object@diagonal)
    upper_probability_lag <- upper_probability(opportunity_cumsum = opportunity_cumsum_lag,
                                               opportunity_diagonal = opportunity_diagonal,
                                               parameters = object@parameters,
                                               diagonal = object@diagonal)
    upper_probability_last <- upper_probability(opportunity_cumsum = opportunity_cumsum_last,
                                                opportunity_diagonal = opportunity_diagonal,
                                                parameters = object@parameters,
                                                diagonal = object@diagonal)

    probability <- (upper_probability_lag - upper_probability) / (1 - upper_probability_last)
    if (!object@diagonal) {
      dibble::diag(probability) <- 0
    }
    probability
  }
  upper_probability <- switch(
    type,
    exponential = function(opportunity_cumsum, opportunity_diagonal, parameters, diagonal) {
      if (!diagonal) {
        opportunity_cumsum <- dibble::broadcast(opportunity_cumsum - opportunity_diagonal,
                                                dim_names = c("origin", "destination"))
      }
      exp(-parameters * opportunity_cumsum)
    },
    power = function(opportunity_cumsum, opportunity_diagonal, parameters, diagonal) {
      dibble::broadcast((opportunity_diagonal / opportunity_cumsum) ^ parameters,
                        dim_names = c("origin", "destination"))
    },
    radiation = function(opportunity_cumsum, opportunity_diagonal, parameters, diagonal) {
      dibble::broadcast(opportunity_diagonal / opportunity_cumsum,
                        dim_names = c("origin", "destination"))
    }
  )
  n_parameters <- switch(
    type,
    exponential = 1L,
    power = 1L,
    radiation = 0L
  )

  InterveningOpportunitiesModel(diagonal = diagonal,
                                fun_parameters = exp,
                                n_parameters = n_parameters,
                                parameters = parameters,
                                probability = probability,
                                upper_probability = upper_probability)
}

# @include mobility_model.R
InterveningOpportunitiesModel <- S7::new_class(
  "InterveningOpportunitiesModel",
  parent = MobilityModel,
  properties = list(
    upper_probability = S7::class_function
  ),
  validator = function(self) {
    fmls_names_upper_probability <- c("opportunity_cumsum", "opportunity_diagonal", "parameters", "diagonal")
    if (!setequal(rlang::fn_fmls_names(self@upper_probability), fmls_names_upper_probability)) {
      cli::cli_abort("{.fn upper_probability} must have arguments {.arg {fmls_names_upper_probability}}.")
    }
  }
)
