apply_by_distance <- function(x, distance, f, ...,
                              decreasing = FALSE) {
  dibble::diag(distance) <- -Inf
  order_distance <- order_distance_by_row(distance, decreasing)

  applied <- init_apply_by_distance(x)
  for(i in vctrs::vec_seq_along(distance)) {
    applied[i, ][order_distance[i, ]] <- f(x[i, ][order_distance[i, ]], ...)
  }
  applied
}

init_apply_by_distance <- function(x) {
  dibble::dibble(NA_real_,
                 .dim_names = dimnames(x))
}

order_distance_by_row <- function(distance, decreasing) {
  order_distance <- init_apply_by_distance(distance)
  for (i in vctrs::vec_seq_along(distance)) {
    order_distance[i, ] <- order(distance[i, ],
                                 decreasing = decreasing)
  }
  order_distance
}
