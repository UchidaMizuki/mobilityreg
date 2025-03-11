#' @export
exploring_opportunities_model <- function(diagonal, exploration_type, deterrence_type) {
  exploration_type <- rlang::arg_match(exploration_type, c("opportunity", "distance"))
  deterrence_type <- rlang::arg_match(deterrence_type, c("exponential", "power_law"))

  if (exploration_type == "opportunity") {
    probability <- function(object, data) {
      purrr::map(
        data,
        \(data) {
          opportunity_exploration <- dplyr::lag(data$relevance, default = 0)
          ExploringOpportunitiesModel_probability(object = object,
                                                  data = data,
                                                  opportunity_exploration = opportunity_exploration)
        }
      )
    }
  } else if (exploration_type == "distance") {
    probability <- function(object, data) {
      purrr::map(
        data,
        \(data) {
          distance_lag <- dplyr::lag(data$distance, default = 0)
          opportunity_exploration <- data$distance - distance_lag
          ExploringOpportunitiesModel_probability(object = object,
                                                  data = data,
                                                  opportunity_exploration = opportunity_exploration)
        }
      )
    }
  }

  upper_probability <- switch(
    deterrence_type,
    exponential = function(opportunity_cumsum, opportunity_origin, deterrence, diagonal) {
      if (!diagonal) {
        opportunity_cumsum <- opportunity_cumsum - opportunity_origin
      }
      exp(-deterrence * opportunity_cumsum)
    },
    power_law = function(opportunity_cumsum, opportunity_origin, deterrence, diagonal) {
      if (diagonal) {
        opportunity_cumsum <- pmax(opportunity_cumsum, opportunity_origin)
      }
      (opportunity_origin / opportunity_cumsum) ^ deterrence
    }
  )

  ExploringOpportunitiesModel(diagonal = diagonal,
                              probability = probability,
                              upper_probability = upper_probability)
}

#' @include mobility_model.R
ExploringOpportunitiesModel <- S7::new_class(
  "ExploringOpportunitiesModel",
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

ExploringOpportunitiesModel_probability <- function(object,
                                                    data,
                                                    opportunity_exploration) {
  opportunity_cumsum_exploration <- cumsum(opportunity_exploration)
  opportunity_cumsum_lead_exploration <- dplyr::lead(opportunity_cumsum_exploration, default = Inf)

  opportunity_origin_exploration <- dplyr::lead(opportunity_cumsum_exploration, default = Inf)
  opportunity_origin_exploration <- dplyr::first(opportunity_origin_exploration)

  upper_probability_exploration <- object@upper_probability(opportunity_cumsum = opportunity_cumsum_exploration,
                                                            opportunity_origin = opportunity_origin_exploration,
                                                            deterrence = data$deterrence,
                                                            diagonal = object@diagonal)
  upper_probability_lead_exploration <- object@upper_probability(opportunity_cumsum = opportunity_cumsum_lead_exploration,
                                                                 opportunity_origin = opportunity_origin_exploration,
                                                                 deterrence = data$deterrence,
                                                                 diagonal = object@diagonal)

  if (!object@diagonal) {
    data$relevance[[1]] <- 0
  }
  opportunity <- data$relevance
  opportunity_cumsum <- cumsum(data$relevance)

  probability <- 1 / opportunity_cumsum * (upper_probability_exploration - upper_probability_lead_exploration)

  if (!object@diagonal) {
    probability[[1]] <- 0
  }
  probability <- rev(cumsum(rev(probability)))
  probability * opportunity
}
