#' @export
intervening_opportunities_model <- function(diagonal, deterrence_type) {
  deterrence_type <- rlang::arg_match(deterrence_type, c("exponential", "power_law", "radiation"))

  if (deterrence_type %in% c("power_law", "radiation") && diagonal) {
    cli::cli_abort('If {.arg deterrence_type} is {.code "power_law"} or {.code "radiation"}, {.arg diagonal} must be {.code FALSE}.')
  }

  probability <- function(object, data) {
    opportunity_cumsum <- apply_by_distance(data$relevance, data$distance, cumsum)
    opportunity_cumsum_lag <- apply_by_distance(opportunity_cumsum, data$distance, dplyr::lag,
                                                default = 0)
    opportunity_cumsum_last <- apply_by_distance(opportunity_cumsum, data$distance, dplyr::last)
    opportunity_origin <- dibble::diag(data$relevance,
                                       axes = "origin")

    upper_probability <- object@upper_probability(opportunity_cumsum = opportunity_cumsum,
                                                  opportunity_origin = opportunity_origin,
                                                  deterrence = data$deterrence,
                                                  diagonal = object@diagonal)
    upper_probability_lag <- object@upper_probability(opportunity_cumsum = opportunity_cumsum_lag,
                                                      opportunity_origin = opportunity_origin,
                                                      deterrence = data$deterrence,
                                                      diagonal = object@diagonal)
    upper_probability_last <- object@upper_probability(opportunity_cumsum = opportunity_cumsum_last,
                                                       opportunity_origin = opportunity_origin,
                                                       deterrence = data$deterrence,
                                                       diagonal = object@diagonal)

    probability <- (upper_probability_lag - upper_probability) / (1 - upper_probability_last)
    if (!object@diagonal) {
      dibble::diag(probability) <- 0
    }
    probability
  }
  upper_probability <- switch(
    deterrence_type,
    exponential = function(opportunity_cumsum, opportunity_origin, deterrence, diagonal) {
      if (!diagonal) {
        opportunity_cumsum <- dibble::broadcast(opportunity_cumsum - opportunity_origin,
                                                dim_names = c("origin", "destination"))
      }
      exp(-deterrence * opportunity_cumsum)
    },
    power_law = function(opportunity_cumsum, opportunity_origin, deterrence, diagonal) {
      dibble::broadcast((opportunity_origin / opportunity_cumsum) ^ deterrence,
                        dim_names = c("origin", "destination"))
    },
    radiation = function(opportunity_cumsum, opportunity_origin, deterrence, diagonal) {
      dibble::broadcast((opportunity_origin * deterrence) / (opportunity_cumsum - opportunity_origin * (1 - deterrence)),
                        dim_names = c("origin", "destination"))
    }
  )

  InterveningOpportunitiesModel(diagonal = diagonal,
                                probability = probability,
                                upper_probability = upper_probability)
}

#' @include mobility_model.R
InterveningOpportunitiesModel <- S7::new_class(
  "InterveningOpportunitiesModel",
  parent = MobilityModel,
  properties = list(
    upper_probability = S7::class_function
  ),
  validator = function(self) {
    fmls_names_upper_probability <- c("opportunity_cumsum", "opportunity_origin", "deterrence", "diagonal")
    if (!setequal(rlang::fn_fmls_names(self@upper_probability), fmls_names_upper_probability)) {
      cli::cli_abort("{.fn upper_probability} must have arguments {.arg {fmls_names_upper_probability}}.")
    }
  }
)
