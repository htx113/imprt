#' Compute Test Statistic
#'
#' This function calculates the absolute difference between the means of
#' the observed outcomes for the two groups specified in the null hypothesis.
#'
#' @param null A vector of two values representing two exposure groups 
#' specified in the null hypothesis.
#' It should specify the two levels of the 'exposure' variable that are being 
#' compared.
#' @param exposure A vector indicating the exposure status for each observation.
#' The values in this vector should contain the levels provided in the 'null' 
#' vector.
#' @param observation A numeric vector of observed values. The function will 
#' compute the means
#' of this vector for the two exposure groups and return the absolute 
#' difference.
#'
#' @return The absolute difference between the means of the 'observation' 
#' values for the two
#' exposure groups specified in 'null'.
#' @export
test_statistic <- function(null,exposure,observation){
  stat <- abs(mean(observation[exposure == null[1]]) -
                mean(observation[exposure == null[2]]))
  return(stat)
}
