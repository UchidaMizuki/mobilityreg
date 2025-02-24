#' @export
exploring_opportunities_model <- function(diagonal, exploration_type, deterrence_type) {
  exploration_type <- rlang::arg_match(exploration_type, c("opportunity", "distance"))
  deterrence_type <- rlang::arg_match(deterrence_type, c("exponential", "power_law"))

  if (exploration_type == "opportunity") {
    probability <- function(object, data) {
      opportunity_exploration <- apply_by_distance(data$relevance, data$distance, dplyr::lag,
                                                   default = 0)
      ExploringOpportunitiesModel_probability(object = object,
                                              data = data,
                                              opportunity_exploration = opportunity_exploration)
    }
  } else if (exploration_type == "distance") {
    probability <- function(object, data) {
      distance_lag <- apply_by_distance(data$distance, data$distance, dplyr::lag,
                                        default = 0)
      opportunity_exploration <- data$distance - distance_lag
      ExploringOpportunitiesModel_probability(object = object,
                                              data = data,
                                              opportunity_exploration = opportunity_exploration)
    }
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
      if (diagonal) {
        opportunity_cumsum <- dibble::broadcast(dibble::pmax(opportunity_cumsum, opportunity_origin),
                                                dim_names = c("origin", "destination"))
      }
      dibble::broadcast((opportunity_origin / opportunity_cumsum) ^ deterrence,
                        dim_names = c("origin", "destination"))
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
  opportunity_cumsum_exploration <- apply_by_distance(opportunity_exploration, data$distance, cumsum)
  opportunity_cumsum_lead_exploration <- apply_by_distance(opportunity_cumsum_exploration, data$distance, dplyr::lead,
                                                           default = Inf)

  opportunity_origin_exploration <- apply_by_distance(opportunity_exploration, data$distance, dplyr::lead,
                                                      default = Inf)
  opportunity_origin_exploration <- dibble::diag(opportunity_origin_exploration,
                                                 axes = "origin")

  upper_probability_exploration <- object@upper_probability(opportunity_cumsum = opportunity_cumsum_exploration,
                                                            opportunity_origin = opportunity_origin_exploration,
                                                            deterrence = data$deterrence,
                                                            diagonal = object@diagonal)
  upper_probability_lead_exploration <- object@upper_probability(opportunity_cumsum = opportunity_cumsum_lead_exploration,
                                                                 opportunity_origin = opportunity_origin_exploration,
                                                                 deterrence = data$deterrence,
                                                                 diagonal = object@diagonal)

  if (!object@diagonal) {
    dibble::diag(data$relevance) <- 0
  }
  opportunity <- data$relevance
  opportunity_cumsum <- apply_by_distance(data$relevance, data$distance, cumsum)

  probability <- dibble::broadcast(1 / opportunity_cumsum * (upper_probability_exploration - upper_probability_lead_exploration),
                                   dim_names = c("origin", "destination"))
  if (!object@diagonal) {
    dibble::diag(probability) <- 0
  }
  probability <- apply_by_distance(probability, data$distance, cumsum,
                                   decreasing = TRUE)
  dibble::broadcast(probability * opportunity,
                    dim_names = c("origin", "destination"))
}
