#' Imputation with Empirical Distribution of Observed Outcomes
#'
#' This function performs imputation for missing observations
#' by sampling from the observed data.
#'
#' @param null A vector of two values representing two exposure groups specified
#' in the null hypothesis.
#' It should specify the two levels of the 'exposure' variable that are being
#' compared.
#' @param exposure A vector indicating the exposure status for each observation.
#' The values in this vector should contain the levels provided in the 'null'
#' vector.
#' @param observation A numeric vector of observed values.
#'
#' @return A vector of imputed values sampled from the observed data.
#' @export
Imputation_sample <- function(null,exposure,observation){
  n <- length(exposure)
  n_obs <- sum(exposure %in% null)
  n_mis <- n - n_obs
  data_obs <- observation[which(exposure %in% null)]
  imp <- sample(data_obs,n_mis,replace=T)
  return(imp)
}
