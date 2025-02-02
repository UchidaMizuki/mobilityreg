apply_by_distance <- function(x, distance, f, ...) {
  applied <- dibble::dibble(NA_real_,
                            .dim_names = dimnames(x))

  for(i in vctrs::vec_seq_along(distance)) {
    order_distance <- order(distance[i, ])

    applied[i, ][order_distance] <- f(x[i, ][order_distance], ...)
  }
  applied
}
