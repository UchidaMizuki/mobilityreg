#' @export
gravity_model <- function(diagonal, type,
                          parameters = NA_real_) {
  type <- rlang::arg_match(type, c("exponential", "power"))

  probability <- function(object, data) {
    x <- data$x
    if (!object@diagonal) {
      dibble::diag(x) <- 0
    }

    distance_decay <- object@distance_decay(data$distance, object@parameters)

    probability <- dibble::ifelse(x == 0, 0, x * distance_decay)
    probability_sum <- dibble::apply(probability, "origin", sum)

    dibble::broadcast(probability / probability_sum,
                      dim_names = c("origin", "destination"))
  }
  distance_decay <- switch(
    type,
    exponential = function(distance, parameters) exp(-parameters * distance),
    power = function(distance, parameters) distance ^ -parameters
  )

  GravityModel(diagonal = diagonal,
               fun_parameters = exp,
               n_parameters = 1L,
               parameters = parameters,
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
    fmls_names_distance_decay <- c("distance", "parameters")
    if (!setequal(rlang::fn_fmls_names(self@distance_decay), fmls_names_distance_decay)) {
      cli::cli_abort("{.fn distance_decay} must have arguments {.arg {fmls_names_distance_decay}}.")
    }
  }
)
