#' Stability Metric for Predator-Prey Model Output
#' 
#' evaluates the stability of predator and prey populations over the final 10 years of a simulation; stability is defined by two conditions: 
#'        (1) the standard deviation of biomass remains within a specified fraction of the mean, and 
#'        (2) the population stays above a defined minimum threshold for a required proportion of years.
#'
#' @param output a data frame from a predator-prey simulation function with columns \code{prey} and \code{predator}
#' @param V_min minimum acceptable prey biomass threshold
#' @param P_min minimum acceptable predator biomass threshold
#' @param mean_prey long-term average or target mean prey biomass
#' @param mean_predator long-term average or target mean predator biomass
#' @param prop_years proportion of the last 10 years that populations must remain above the threshold to be considered stable (default is 0.6)
#' @param sd_fraction maximum allowed standard deviation as a fraction of the mean (default is 0.4)
#' @author Jaden Orli
#' @return returns \code{1} if both prey and predator populations meet the stability criteria, otherwise returns \code{0}

#write a function to assess the stability of a predator-prey simulation
stability_metric <- function(output, V_min, P_min, mean_prey, mean_predator,
                             prop_years = 0.6, sd_fraction = 0.4) {

  #extract the last 10 years of simulation data for analysis
  last_10 <- tail(output, 10)
  
  #calculate the allowable standard deviation thresholds (as fractions of the mean)
  sd_thresh_V <- sd_fraction * mean_prey
  sd_thresh_P <- sd_fraction * mean_predator
  
  #compute the standard deviations for the prey and predator over the last 10 years
  sd_V <- sd(last_10$prey, na.rm = TRUE)
  sd_P <- sd(last_10$predator, na.rm = TRUE)
  
  #compute the proportion of years in which prey and predator populations are above the defined minimums
  prop_V_above <- mean(last_10$prey > V_min, na.rm = TRUE)
  prop_P_above <- mean(last_10$predator > P_min, na.rm = TRUE)
  
  #determine whether biomass is stable: must stay within SD threshold and above floor for enough years
  V_stable <- (sd_V < sd_thresh_V) && (prop_V_above >= prop_years)
  P_stable <- (sd_P < sd_thresh_P) && (prop_P_above >= prop_years)
  
  #return 1 if both populations are stable, otherwise return 0
  return(as.numeric(V_stable && P_stable))
}
