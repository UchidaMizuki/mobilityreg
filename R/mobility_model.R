MobilityModel <- S7::new_class(
  "MobilityModel",
  properties = list(
    diagonal = S7::class_logical,
    probability = S7::class_function
  ),
  validator = function(self) {
    vctrs::vec_check_size(self@diagonal, 1)

    fmls_names_probability <- c("object", "data")
    if (!setequal(rlang::fn_fmls_names(self@probability), fmls_names_probability)) {
      cli::cli_abort("{.fn probability} must have arguments {.arg {fmls_names_probability}}.")
    }
  }
)

S7::method(predict, MobilityModel) <- function(object, new_data,
                                               class = c("vector", "dibble")) {
  class <- rlang::arg_match(class, c("vector", "dibble"))

  data <- MobilityModel_probability(object, new_data)
  switch(
    class,
    vector = MobilityModel_as_vector(data, new_data),
    dibble = data
  )
}

MobilityModel_as_dibble <- function(data) {
  if (dibble::is_dibble(data)) {
    return(data)
  }
  data <- dibble::dibble_by(data, "origin", "destination")

  dim_names <- dimnames(data)
  location <- unique(dim_names$origin, dim_names$destination)
  dim_names <- list(origin = location,
                    destination = location)

  data <- dibble::broadcast(data, dim_names)
  tidyr::replace_na(data, list(distance = 0, x = 0))
}

MobilityModel_probability <- function(object, data) {
  data <- MobilityModel_as_dibble(data)

  object@probability(object = object,
                     data = data)
}

MobilityModel_as_vector <- function(data, new_data) {
  data <- tibble::as_tibble(data)

  new_data <- new_data[c("origin", "destination")]
  new_data <- dplyr::left_join(new_data, data,
                               by = c("origin", "destination"))
  new_data$.
}
