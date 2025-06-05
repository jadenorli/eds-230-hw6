#' Lotka-Volterra (Scenario One: Biomass-Proportional Harvest) Model Simulation
#'
#' simulates the dynamics of a predator-prey system using a discrete-time Lotka-Volterra model with logistic prey growth, predator carrying capacity, and proportional prey harvest (biomass-based hunting)
#' 
#' @param initial_states a named list with two elements: \code{V0} (initial prey biomass) and \code{P0} (initial predator biomass)
#' @param years number of years to simulate
#' @param params a named list of parameters:
#' \itemize{
#'   \item \code{b}: prey intrinsic growth rate
#'   \item \code{K_v}: prey carrying capacity
#'   \item \code{K_p}: predator carrying capacity
#'   \item \code{alpha}: predator attack rate on prey
#'   \item \code{c}: conversion efficiency of prey into predator biomass
#'   \item \code{d}: predator mortality rate
#'   \item \code{h}: proportional harvest rate of prey (biomass-based hunting)
#' }
#' @param dt the time step in days (default = 1).
#' @author Jaden Orli
#' @return a data frame with columns \code{year}, \code{prey}, and \code{predator}

#write a function to simulate a predator-prey model and return the final population size for each year
lotka_volterra1 <- function(initial_states, years, params, dt = 1) {
  
  #unpack the parameters
  b <- params$b #prey intrinsic growth rate
  K_v <- params$K_v #prey carrying capacity
  K_p <- params$K_p #predator carrying capacity
  alpha <- params$alpha #predator attack rate on prey
  c <- params$c #predator conversion efficiency 
  d <- params$d #predator mortality rate
  h <- params$h #hunting rate
  
  #unpack the initial conditions
  V0 <- initial_states$V0 #initial prey population
  P0 <- initial_states$P0 #initial predator population
  
  #define the initial conditions
  V_init <- V0 #initial prey population size
  P_init <- P0 #initial predator population size
  
  #define the number of day in a year
  days_per_year <- 365
  
  #calculate the number of timesteps in a year 
  steps_per_year <- days_per_year / dt
  
  #calculate the time within a year 
  time_within_year <- seq(0, days_per_year, by = dt)
  
  #initialize a list to store final values per year
  final_vals <- data.frame(year = integer(), 
                           prey = numeric(), 
                           predator = numeric())
  
  #for each year in the years
  for (year in 1:years) {
    
    #create initial vectors for this year
    V <- numeric(length(time_within_year)) #prey vector for this year
    P <- numeric(length(time_within_year)) #predator vector for this year
    
    #store the initial conditions in the first spot of the vector
    V[1] <- V_init #initial prey 
    P[1] <- P_init #initial predator 
    
    #run the simulation for each year
    for (i in 2:length(time_within_year)) {
      
      #biomass proportional harvest 
      hunting <- h * V[i - 1]
      
      #calculate the growth of the prey population and ensure that it is not negative 
      prey_growth <- b * V[i - 1] * max(0, 1 - V[i - 1] / K_v) 
      
      #calculate the change in the prey population for the year
      dV <- ((prey_growth - alpha * P[i - 1] * V[i - 1]) - hunting) * dt
      
      #calculate the growth of the predator population
      predator_growth <- c * alpha * P[i - 1] * V[i - 1]
      
      #ensure that the predator population does not experience negative growth
      predator_capacity <- max(0, 1 - P[i - 1] / K_p) 
      
      #calculate the change in the predator population for the year
      dP <- (predator_growth * predator_capacity - d * P[i - 1]) * dt
      
      #store the final prey population 
      V[i] <- max(V[i - 1] + dV, 0)
      
      #store the final predator population
      P[i] <- max(P[i - 1] + dP, 0)
    }
    
    #append the final prey and predator values of the year
    final_vals <- rbind(final_vals, data.frame(year = year,
                                               prey = V[length(V)],
                                               predator = P[length(P)]))
    
    #update the initial values for next year
    V_init <- V[length(V)] #new initial prey population
    P_init <- P[length(P)] #new initial predator population
  }
  
  #return the dataframe of final values for the prey and predator populations
  return(final_vals)
}
