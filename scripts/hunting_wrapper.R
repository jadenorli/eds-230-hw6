#'
#'
#'
#'
#'
#'

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
