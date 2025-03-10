MobilityModel_2 <- S7::new_class(
  "MobilityModel_2",
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

S7::method(predict, MobilityModel_2) <- function(object, new_data,
                                                 class = c("vector", "list")) {
  class <- rlang::arg_match(class, c("vector", "list"))

  probability <- object@probability(object = object,
                                    data = new_data)
  switch(
    class,
    vector = MobilityModel_2_as_vector(probability, new_data),
    list = probability
  )
}

MobilityModel_2_as_vector <- function(probability, new_data) {
  if (!all(vctrs::list_sizes(probability) == vctrs::list_sizes(new_data))) {
    cli::cli_abort("The length of {.arg probability} must be the same as the length of {.arg new_data}.")
  }

  probability <- vctrs::list_unchop(probability)

  new_data <- vctrs::list_unchop(new_data)
  new_data$probability <- probability
  new_data <- dplyr::arrange(new_data, .data$id)
  new_data$probability
}
