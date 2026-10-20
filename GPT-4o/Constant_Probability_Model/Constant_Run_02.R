# Store transition probabilities
tp <- list(
  standard = list(
    stable_to_progressive = 0.0956,
    stable_to_death = 0.0464,
    progressive_to_death = 0.0861
  ),
  long_term = list(
    stable_to_progressive = 0.0894,
    stable_to_death = 0.0397,
    progressive_to_death = 0.0689
  )
)

# Store health utility values
utility <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Store cost data
costs <- list(
  standard = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 350,
    progressive = 2720,
    death = 0
  ),
  long_term = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 2453,
    progressive = 2720,
    death = 0
  )
)

# Define model parameters
cycle_length <- 1  # in months
time_horizon <- 60 # in months

# Function to compute Markov trace
compute_trace <- function(tp, cycles) {
  # Initialize Markov trace
  trace <- matrix(0, nrow = cycles, ncol = 3)
  colnames(trace) <- c("Stable", "Progressive", "Death")
  trace[1, ] <- c(1, 0, 0)  # Start with 100% in stable state
  
  # Iterate through cycles
  for (t in 2:cycles) {
    trace[t, 1] <- trace[t - 1, 1] * (1 - tp$stable_to_progressive - tp$stable_to_death)
    trace[t, 2] <- trace[t - 1, 1] * tp$stable_to_progressive + trace[t - 1, 2] * (1 - tp$progressive_to_death)
    trace[t, 3] <- trace[t - 1, 1] * tp$stable_to_death + trace[t - 1, 2] * tp$progressive_to_death + trace[t - 1, 3]
  }
  
  return(trace)
}

# Compute Markov traces for both treatments
trace_standard <- compute_trace(tp$standard, time_horizon)
trace_long_term <- compute_trace(tp$long_term, time_horizon)

# Store results
trace_results <- list(
  standard = trace_standard,
  long_term = trace_long_term
)

# Function to compute QALMs
compute_qalms <- function(trace, utility, cycles) {
  qalms <- numeric(cycles)
  progressive_utilities <- rep(utility$progressive, cycles)
  
  # Apply decrement to progressive state utility from cycle 3 to 25
  for (t in 3:25) {
    progressive_utilities[t] <- max(progressive_utilities[t - 1] - utility$decrement_per_month, 0)
  }
  
  # Keep utility fixed after cycle 25
  progressive_utilities[26:cycles] <- progressive_utilities[25]
  
  # Compute QALMs per cycle
  for (t in 1:cycles) {
    qalms[t] <- trace[t, 1] * utility$stable + trace[t, 2] * progressive_utilities[t]
  }
  
  return(qalms)
}

# Compute QALMs for both treatments
qalms_standard <- compute_qalms(trace_standard, utility, time_horizon)
qalms_long_term <- compute_qalms(trace_long_term, utility, time_horizon)

# Compute total QALMs
total_qalms_standard <- sum(qalms_standard)
total_qalms_long_term <- sum(qalms_long_term)

# Compute total QALYs
total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Function to compute costs per cycle
compute_costs <- function(trace, costs, cycles) {
  cost_per_cycle <- numeric(cycles)
  
  for (t in 1:cycles) {
    stable_cost <- ifelse(t <= 4, costs$stable_1_3,
                          ifelse(t <= 7, costs$stable_4_6, costs$stable_subsequent))
    
    cost_per_cycle[t] <- trace[t, 1] * stable_cost + trace[t, 2] * costs$progressive + trace[t, 3] * costs$death
  }
  
  return(cost_per_cycle)
}

# Compute costs for both treatments
costs_standard <- compute_costs(trace_standard, costs$standard, time_horizon)
costs_long_term <- compute_costs(trace_long_term, costs$long_term, time_horizon)

# Compute total costs
total_costs_standard <- sum(costs_standard)
total_costs_long_term <- sum(costs_long_term)

# Compute ICERs
icer_qalms <- round((total_costs_long_term - total_costs_standard) / (total_qalms_long_term - total_qalms_standard))
icer_qalys <- round((total_costs_long_term - total_costs_standard) / (total_qalys_long_term - total_qalys_standard))

# Store costs results
costs_results <- list(
  standard = costs_standard,
  long_term = costs_long_term,
  total_standard = total_costs_standard,
  total_long_term = total_costs_long_term
)

# Store final results
results <- list(
  qalms = list(
    standard = qalms_standard,
    long_term = qalms_long_term,
    total_standard = total_qalms_standard,
    total_long_term = total_qalms_long_term,
    total_qalys_standard = total_qalys_standard,
    total_qalys_long_term = total_qalys_long_term
  ),
  costs = costs_results,
  icer = list(
    qalms = nicer_qalms,
    qalys = nicer_qalys
  )
)
