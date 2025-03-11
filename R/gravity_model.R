#' @export
gravity_model <- function(diagonal, deterrence_type) {
  deterrence_type <- rlang::arg_match(deterrence_type, c("exponential", "power_law"))

  probability <- function(object, data) {
    purrr::map(
      data,
      \(data) {
        relevance <- data$relevance
        if (!object@diagonal) {
          relevance[[1]] <- 0
        }

        distance_decay <- object@distance_decay(data$distance, data$deterrence)

        probability <- ifelse(relevance == 0, 0, relevance * distance_decay)
        probability / sum(probability)
      }
    )
  }
  distance_decay <- switch(
    deterrence_type,
    exponential = function(distance, deterrence) exp(-deterrence * distance),
    power_law = function(distance, deterrence) distance ^ -deterrence
  )

  GravityModel(diagonal = diagonal,
               probability = probability,
               distance_decay = distance_decay)
}

#' @include mobility_model.R
GravityModel <- S7::new_class(
  "GravityModel",
  parent = MobilityModel,
  properties = list(
    distance_decay = S7::class_function
  ),
  validator = function(self) {
    fmls_names_distance_decay <- c("distance", "deterrence")
    if (!setequal(rlang::fn_fmls_names(self@distance_decay), fmls_names_distance_decay)) {
      cli::cli_abort("{.fn distance_decay} must have arguments {.arg {fmls_names_distance_decay}}.")
    }
  }
)
