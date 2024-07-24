library("Rsolnp")

# Define the objective function
objective_function <- function(v) {
  # Calculate traditional fuel consumption
  traditional_fuel_consumption <- sum((0.006705 * v^3 + 87.71)*(1-0.3))
  # Calculate alternative fuel consumption based on the fraction alpha
  alpha <- 0.3
  alternative_fuel_consumption <- sum((0.005000 * v^3 + 75.00) * alpha)
  # Total CO2 emissions from both types of fuels
  co2_emissions <- 3.110 * traditional_fuel_consumption + 1.500 * alternative_fuel_consumption
  return(co2_emissions)
}


# Define the inequality constraint function
inequality_constraint <- function(v) {
  constraints <- numeric(118 + 2 + 7) # Add space for new constraints (120 existing + 3 new + 6 reduction zones)
  
  for (i in 1:48) {
    constraints[i] <- 18 - v[i]
    constraints[i + 48] <- v[i] - 25
  }
  
  sum_of_travellingtime <- sum(1134.5 / v)
  
  constraints[97] <- sum_of_travellingtime - 142.2
  constraints[98] <- sum_of_travellingtime - 110.5
  constraints[99] <- sum_of_travellingtime - 68.3
  constraints[100] <- sum_of_travellingtime - 69.6
  constraints[101] <- sum_of_travellingtime - 52.2
  constraints[102] <- sum_of_travellingtime - 18.3
  constraints[103] <- sum_of_travellingtime - 24.4
  constraints[104] <- sum_of_travellingtime - 21.6
  constraints[105] <- sum_of_travellingtime - 20.4
  constraints[106] <- sum_of_travellingtime - 14.2
  constraints[107] <- sum_of_travellingtime - 24.5
  constraints[108] <- sum_of_travellingtime - 15.3
  constraints[109] <- sum_of_travellingtime - 41
  constraints[110] <- sum_of_travellingtime - 66.1
  constraints[111] <- sum_of_travellingtime - 63.8
  constraints[112] <- sum_of_travellingtime - 95.6
  
  # Alternative Fuel Usage Constraint
  alpha <- 0.3 # Minimum percentage of alternative fuel usage
  total_fuel_consumption <- sum(0.006705 * v^3 + 87.71)
  alt_fuel_consumption <- sum(0.005000 * v^3 + 75.00) # Assuming all alternative fuel
  
  # Constraint to ensure at least 30% of total fuel consumption is from alternative fuels
  min_alt_fuel_usage <- alpha * total_fuel_consumption
  constraints[113] <- alt_fuel_consumption - min_alt_fuel_usage
  
  # Energy Efficiency Constraint (EEDI)
  SFC <- 200 # Specific Fuel Consumption (g/kWh), assumed value
  P_ME <- 10000 # Main Engine Power (kW), assumed value
  capacity <- 200000
  EEDI_reduction_factor <-  # 20% reduction
  baseline_EEDI_reference <- 961.79 * capacity^-0.477
  co2_emissions <- 3.110 * sum((0.006705 * v^3 + 87.71) * (1 - alpha)) + 1.500 * sum((0.005000 * v^3 + 75.00) * alpha)
  eedi <- (co2_emissions * SFC * P_ME) / (capacity * mean(v)) # EEDI formula based on provided image
  eedi_target <- baseline_EEDI_reference * (1 - EEDI_reduction_factor)
  constraints[114] <- eedi - eedi_target
  
  
  # Renewable Energy Usage (example constraint)
  min_fuel_efficiency <- 1
  total_distance <- 17704.1
  fuel_efficiency <- total_fuel_consumption / total_distance
  constraints[115] <- min_fuel_efficiency - fuel_efficiency
  
  # Maximum Fuel Consumption per Trip (example constraint)
  max_fuel_per_trip <- 5000 # Example limit
  constraints[116] <- total_fuel_consumption - max_fuel_per_trip
  
  
  # Total Emissions Cap (example constraint)
  total_emissions_cap <- 15000 # Example limit
  total_emissions <- 3.110 * total_fuel_consumption # Combined emissions
  constraints[117] <- total_emissions - total_emissions_cap
  
  
  # Speed Reduction Zones (example constraint)
  speed_reduction_zone_speed <- 16 # Max speed in reduction zones
  reduction_zone_segments <- c(1, 2, 3, 4, 5, 6,7) # Segments in speed reduction zones
  for (i in seq_along(reduction_zone_segments)) {
    constraints[118 + i] <- v[reduction_zone_segments[i]] - speed_reduction_zone_speed
  }
  
  return(constraints)
}

initial_guess <- c(18,16,22,19,20,14,19,24,20,22,21,24,16,20,17,19,20,14,15,14,18,16,22,19,20,14,19,24,20,22,21,24,16,20,17,19,20,14,15,14,
                   18,16,16,17,19,20,16,19)

opt <- solnp(par = initial_guess,
             fun = objective_function,
             ineqfun = inequality_constraint,
             ineqLB = c(rep(-7, 112), 0, -Inf,  -Inf, 0, 0, 0, 0, 0, -2,-2,-2,-2,-2,-2,-2 ), # Add bounds for new constraints
             ineqUB = c(rep(0, 112), Inf, 0,  0, Inf, Inf, Inf, Inf, Inf,  0,0,0,0,0,0,0), # Add bounds for new constraints
             LB = rep(18, 48),
             UB = rep(25, 48))

# Print the optimization result
print(opt)

cat("Optimal Decision Variables:", opt$pars, "\n")

objective_value <- objective_function(opt$pars)

cat("Objective Function Value: ", objective_value, "\n")
