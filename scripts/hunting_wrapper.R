#' Lotka-Volterra (Hunting: Scenario Three) Model Wrapper Function
#'
#' Lotka-Volterra predator-prey simulation with hunting pressure, using specified ecological parameters and simulation settings
#' 
#' @param h the baseline hunting rate applied when prey biomass exceeds the threshold
#' @param V_thresh the prey biomass threshold below which harvest is halted
#' @param b prey intrinsic growth rate
#' @param alpha attack rate of the predator on the prey
#' @param d natural mortality rate of the predator population
#' @param K_v carrying capacity of the prey population
#' @param K_p carrying capacity of the predator population
#' @param c conversion efficiency of prey biomass into predator biomass
#' @param initial_state initial population sizes, typically named \code{prey} and \code{predator}
#' @param years total number of years to simulate
#' @param V_min minimum acceptable prey biomass to consider the population stable
#' @param P_min minimum acceptable predator biomass to consider the population stable
#' @param mean_prey long-term average prey biomass (used in variability calculation)
#' @param mean_predator long-term average predator biomass (used in variability calculation)
#' @author Jaden Orli
#' @return a single numeric stability score computed using the `stability_metric()` function

#write a function that takes key ecological and management parameters as inputs for Sobol sensitivity analysis 
hunting_wrapper <- function(h, V_thresh, b, alpha, d, K_v, K_p, c,
                            initial_state, years,
                            V_min, P_min, mean_prey, mean_predator) {
  
  #create a list of parameters to pass to the Lotka-Volterra model
  params <- list(h = h, 
                 V_thresh = V_thresh, 
                 b = b, 
                 alpha = alpha,
                 d = d, 
                 K_v = K_v, 
                 K_p = K_p, 
                 c = c)
  
  #run the adaptive management Lotka-Volterra simulation for 50 years
  output <- lotka_volterra3(initial_states = initial_state,
                            years = years,
                            params = params)
  
  #use the stability metric function 
  stability_metric(output = output,
                   V_min = V_min,
                   P_min = P_min,
                   mean_prey = mean_prey,
                   mean_predator = mean_predator,
                   prop_years = 0.6,
                   sd_fraction = 0.4)
}
