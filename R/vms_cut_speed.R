vms_cut_speed <- function(x, fishing.lim) {

  x <- if_else(x > 0, x + 1e-12, 0, NA_real_)

  labs <- c("0",
            paste0(">0 - <", fishing.lim[[1]]),
            paste0("\u2265",fishing.lim[[1]]," - \u2264", fishing.lim[[2]]),
            paste0(">", fishing.lim[[2]]))

  cut(x, breaks = c(-Inf, 0, fishing.lim[[1]], fishing.lim[[2]], Inf), labels = labs)

}
