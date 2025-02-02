get_empty_mobility_model <- function() {
  MobilityModel(diagonal = TRUE,
                n_parameters = 0L,
                probability = function(object, data) {})
}

get_data_mobity_model <- function(reg, data) {
  data <- MobilityReg_new_data(reg, data)
  MobilityModel_as_dibble(data)
}
