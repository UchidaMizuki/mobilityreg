.onLoad <- function(...) {
  S7::methods_register()

  init_apply_by_distance <<- memoise::memoise(init_apply_by_distance)
  order_distance_by_row <<- memoise::memoise(order_distance_by_row)
}
