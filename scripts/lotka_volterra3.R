#'
#'
#'
#'
#'
#'
#'

#write a function to simulate a predator-prey model and return the final population size for each year
lotka_volterra3 <- function(initial_states, years, params, dt = 1, recovery_buffer = 2) {
  
  #unpack the parameters
  b <- params$b #prey intrinsic growth rate
  K_v <- params$K_v #prey carrying capacity
  K_p <- params$K_p #predator carrying capacity
  alpha <- params$alpha #predator attack rate on prey
  c <- params$c #predator conversion efficiency 
  d <- params$d #predator mortality rate
  h_base <- params$h #hunting rate
  V_thresh <- params$V_thresh #harvest biomass closure threshold
  
  #unpack the initial conditions
  V0 <- initial_states$V0 #initial prey population
  P0 <- initial_states$P0 #initial predator population
  
  #define the initial conditions
  V_init <- V0 #initial prey population size
  P_init <- P0 #initial predator population size
  V_last <- V0 #memory for adaptive management

  #define the number of day in a year
  days_per_year <- 365
  
  #calculate the number of timesteps in a year 
  steps_per_year <- days_per_year / dt
  
  #calculate the time within a year 
  time_within_year <- seq(0, days_per_year, by = dt)
  
  #initialize a list to store final values per year
  final_vals <- data.frame(year = integer(), 
                           prey = numeric(), 
                           predator = numeric(), 
                           hunting = integer(), 
                           hunt_amount = numeric(),
                           hunt_rate = numeric())
  
  #recovery buffer tracker
  hunting_active <- TRUE
  above_thresh_years <- ifelse(V0 > V_thresh, 1, 0)
  
  #for each year in the years
  for (year in 1:years) {
    
    #reset total hunt tracker each year
    yearly_hunt <- 0 
    
    #if the prey biomass is above the minimum biomass threshold and greater than 2 years of recovery
    if (above_thresh_years >= recovery_buffer) {
      
      #then set hunting to be active
      hunting_active <- TRUE
      
      #if the biomass is not above the minimum, 
    } else {
      
      #then set hunting to be zero
      hunting_active <- FALSE
    }
    
    #track history of past prey values for smoothing (initialize storage if not already)
    if (!exists("prey_history")) prey_history <- numeric()
    
    #update prey history
    prey_history <- c(prey_history, V_init)
    
    #if there are at least two years to compare biomass levels, 
    if (length(prey_history) >= 2) {
      
      #take the last few years of biomass data (last 3 years of less) to create a rolling window 
      recent <- tail(prey_history, min(length(prey_history), 3))
      
      #calculate the relative change in prey population over the rolling window
      rel_change <- (recent[length(recent)] - recent[1]) / recent[1]
      
    #if there aren't enough years yet
    } else {
      
      #then set the relative change to zero 
      rel_change <- 0
    }
    
    #adjust harvest rate based on relative change
    h_t <- h_base * (1 + rel_change)
    
    #cap the harvest rate within biologically realistic limits
    h_t <- max(min(h_t, 0.8), 0)
    
    #create initial vectors for this year
    V <- numeric(length(time_within_year)) #prey vector for this year
    P <- numeric(length(time_within_year)) #predator vector for this year
    
    #store the initial conditions in the first spot of the vector
    V[1] <- V_init #initial prey 
    P[1] <- P_init #initial predator 

    #run the simulation for each year
    for (i in 2:length(time_within_year)) {
      
      #if hunting is active this year, 
      if (hunting_active && V[i - 1] > V_thresh ) {
        
        #biomass proportional harvest 
        hunting <- h_t * V[i - 1]
        
        #if hunting is not active this year,
      } else {
        
        #set hunting to zero
        hunting <- 0
      }
      
      #calculate the yearly hunting allowed
      yearly_hunt <- yearly_hunt + hunting * dt
      
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
                                               predator = P[length(P)],
                                               hunting = as.integer(hunting_active),
                                               hunt_amount = ifelse(hunting_active, yearly_hunt, 0),
                                               hunt_rate = ifelse(hunting_active, h_t, NA))) 
    #update the recovery buffer counter
    if (V[length(V)] > V_thresh) {
      above_thresh_years <- above_thresh_years + 1
    } else {
      above_thresh_years <- 0
    }
    
    #update the initial values for next year
    V_init <- V[length(V)] #new initial prey population
    P_init <- P[length(P)] #new initial predator population
    V_last <- V[length(V)] #new ending prey population 
  }
  
  #return the dataframe of final values for the prey and predator populations
  return(final_vals)
}

