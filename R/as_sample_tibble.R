# as_sample_tibble
#
# Author: mjskay
###############################################################################


#' @importFrom tidybayes as_sample_tibble
#' @export
as_sample_tibble.map = function(model) {
  mu = coef(model)
  samples = as_tibble(mvrnorm(n = 10000, mu = mu, Sigma = vcov(model)))
  #map models have no chains
  bind_cols(data_frame(.chain = 1, .iteration = 1:nrow(samples)), samples)
}

#' @export
as_sample_tibble.map2stan = function(model) {
  as_sample_tibble(model@stanfit)
}
