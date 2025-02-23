get_empty_mobility_model <- function() {
  MobilityModel(diagonal = TRUE,
                probability = function(object, data) {})
}

get_data_mobity_model <- function(reg, data) {
  data <- MobilityReg_new_data(reg, data)
  MobilityModel_as_dibble(data)
}
