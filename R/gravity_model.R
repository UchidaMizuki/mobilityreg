#' @export
gravity_model <- function(diagonal, type) {
  type <- rlang::arg_match(type, c("exponential", "power"))

  probability <- function(object, data) {
    relevance <- data$relevance
    if (!object@diagonal) {
      dibble::diag(relevance) <- 0
    }

    distance_decay <- object@distance_decay(data$distance, data$deterrence)

    probability <- dibble::ifelse(relevance == 0, 0, relevance * distance_decay)
    probability_sum <- dibble::apply(probability, "origin", sum)

    dibble::broadcast(probability / probability_sum,
                      dim_names = c("origin", "destination"))
  }
  distance_decay <- switch(
    type,
    exponential = function(distance, deterrence) exp(-deterrence * distance),
    power = function(distance, deterrence) distance ^ -deterrence
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
