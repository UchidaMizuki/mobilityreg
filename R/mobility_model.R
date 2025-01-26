MobilityModel <- S7::new_class(
  "MobilityModel",
  properties = list(
    probability = S7::class_function,
    n_parameters = S7::class_integer,
    parameters = S7::class_numeric
  ),
  validator = function(self) {
    fmls_names_probability <- rlang::fn_fmls_names(self@probability)
    if (!setequal(rlang::fn_fmls_names(self@probability), fmls_names_probability)) {
      cli::cli_abort("{.fn probability} must have arguments {.arg {fmls_names_probability}}.")
    }
    vctrs::vec_check_size(self@n_parameters, 1)
    vctrs::vec_check_size(self@parameters, self@n_parameters)
  }
)

S7::method(predict, MobilityModel) <- function(object, new_data,
                                               type = c("probability", "flow"),
                                               class = c("vector", "dibble")) {
  type <- rlang::arg_match(type, c("probability", "flow"))
  class <- rlang::arg_match(class, c("vector", "dibble"))

  data <- switch(
    type,
    flow = MobilityModel_flow(object, new_data),
    probability = MobilityModel_probability(object, new_data)
  )
  switch(
    class,
    vector = MobilityModel_as_vector(data, new_data),
    dibble = data
  )
}

MobilityModel_as_dibble <- function(object, data) {
  if (dibble::is_dibble(data)) {
    return(data)
  }
  data <- dibble::dibble_by(data, "origin", "destination")
  tidyr::replace_na(data, list(distance = Inf, x = 0, y = 0))
}

MobilityModel_probability <- function(object, data) {
  data <- MobilityModel_as_dibble(object, data)

  object@probability(object = object,
                     data = data)
}

MobilityModel_flow <- function(object, data) {
  data <- MobilityModel_as_dibble(object, data)

  y_sum <- dibble::apply(data$y, "origin", sum)
  probability <- MobilityModel_probability(object, data)

  dibble::broadcast(y_sum * probability,
                    dim_names = c("origin", "destination"))
}

MobilityModel_as_vector <- function(data, new_data) {
  data <- tibble::as_tibble(data)

  new_data <- dplyr::select(new_data, "origin", "destination")
  new_data <- dplyr::left_join(new_data, data,
                               by = c("origin", "destination"))
  new_data$.
}
