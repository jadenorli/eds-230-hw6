#' Lotka-Volterra (No Hunting) Model Wrapper Function
#'
#' Lotka-Volterra predator-prey simulation without any hunting pressure, using specified ecological parameters and simulation settings
#'
#' @param b intrinsic growth rate of the prey population
#' @param alpha attack rate of the predator on the prey
#' @param d natural mortality rate of the predator population
#' @param K_v carrying capacity of the prey population
#' @param K_p carrying capacity of the predator population
#' @param c conversion efficiency of prey biomass into predator biomass
#' @param initial_state initial population sizes, typically named \code{prey} and \code{predator}
#' @param years total number of years to simulate
#' @author Jaden Orli
#' @return the mean prey biomass over the final 10 years of the simulation as a proxy for long-term population stability

#write a function that takes key ecological and management parameters as inputs for Sobol sensitivity analysis 
no_hunting_wrapper <- function(b, alpha, d, K_v, K_p, c,
                               initial_state, years) {
  
  #create a list of parameters to pass to the Lotka-Volterra model
  params <- list(b = b, 
                 alpha = alpha, 
                 d = d,
                 K_v = K_v, 
                 K_p = K_p, 
                 c = c)
  
  #run the model
  output <- lotka_volterra(initial_states = initial_state,
                           years = years,
                           params = params)
  
  #subset the data to the last 10 years 
  last10 <- tail(output, 10)
  
  #return the average prey biomass
  return(mean(last10$prey)) 
}