# Store input data without printing

# Health utility values
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Cost data
costs <- list(
  standard_TZ = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 350,
    progressive = 2720,
    death = 0
  ),
  long_term_TZ = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 2453,
    progressive = 2720,
    death = 0
  )
)

# Cycle length and time horizon
time_settings <- list(
  cycle_length = 1,  # in months
  time_horizon = 60  # in months
)

# Store Overall Survival (OS) and Progression-Free Survival (PFS) data for Standard TMZ therapy
OS_standard <- c(0.988, 0.970, 0.950, 0.913, 0.888, 0.863, 0.813, 0.782, 0.733, 0.675, 
                 0.650, 0.611, 0.550, 0.525, 0.488, 0.450, 0.430, 0.394, 0.350, 0.325, 
                 0.313, 0.300, 0.275, 0.265, 0.240, 0.225, 0.213, 0.213, 0.213, 0.213, 
                 0.188, 0.188, 0.188, 0.188, 0.188, 0.188, 0.188, 0.188, 0.188, 0.188, 
                 0.094, 0.087, 0.081, 0.075, 0.070, 0.065, 0.060, 0.056, 0.052, 0.048, 
                 0.045, 0.041, 0.038, 0.036, 0.033, 0.030, 0.028, 0.026, 0.024, 0.022)

PFS_standard <- c(0.978, 0.968, 0.800, 0.738, 0.643, 0.539, 0.493, 0.459, 0.417, 0.368, 
                  0.314, 0.269, 0.259, 0.228, 0.227, 0.211, 0.197, 0.184, 0.174, 0.157, 
                  0.146, 0.136, 0.126, 0.107, 0.097, 0.097, 0.097, 0.093, 0.093, 0.093, 
                  0.071, 0.057, 0.033, 0.033, 0.033, 0.033, 0.024, 0.022, 0.020, 0.018, 
                  0.016, 0.014, 0.013, 0.011, 0.010, 0.009, 0.008, 0.007, 0.007, 0.006, 
                  0.005, 0.005, 0.004, 0.004, 0.003, 0.003, 0.003, 0.003, 0.002, 0.002)

# Calculate transition probabilities using the given formula
calc_transition_prob <- function(y, t) {
  1 - exp(log(y) / t)
}

num_cycles <- length(OS_standard)
transition_probs_standard <- data.frame(
  cycle = 1:num_cycles,
  prob_stable_to_death = sapply(1:num_cycles, function(t) calc_transition_prob(OS_standard[t], t)),
  prob_stable_to_progressive = sapply(1:num_cycles, function(t) calc_transition_prob(PFS_standard[t], t)),
  prob_progressive_to_death = sapply(1:num_cycles, function(t) calc_transition_prob(OS_standard[t] - PFS_standard[t], t))
)

# Calculate probabilities for remaining states
transition_probs_standard$prob_stable_to_stable <- 1 - (transition_probs_standard$prob_stable_to_death + transition_probs_standard$prob_stable_to_progressive)
transition_probs_standard$prob_progressive_to_progressive <- 1 - transition_probs_standard$prob_progressive_to_death

# Store transition probabilities without printing

# Store Overall Survival (OS) and Progression-Free Survival (PFS) data for Long-term TMZ therapy
OS_long_term <- c(1.000, 1.000, 0.982, 0.930, 0.895, 0.860, 0.825, 0.789, 0.737, 0.702, 
                  0.667, 0.649, 0.614, 0.579, 0.544, 0.526, 0.491, 0.456, 0.421, 0.404, 
                  0.368, 0.351, 0.351, 0.351, 0.351, 0.333, 0.333, 0.316, 0.316, 0.296, 
                  0.296, 0.296, 0.296, 0.296, 0.296, 0.269, 0.241, 0.215, 0.215, 0.215, 
                  0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 
                  0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215)

PFS_long_term <- c(0.947, 0.912, 0.877, 0.754, 0.699, 0.607, 0.497, 0.478, 0.421, 0.402, 
                   0.364, 0.325, 0.306, 0.306, 0.306, 0.287, 0.268, 0.268, 0.268, 0.268, 
                   0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.244, 
                   0.244, 0.244, 0.244, 0.244, 0.244, 0.244, 0.203, 0.203, 0.203, 0.203, 
                   0.203, 0.203, 0.203, 0.203, 0.203, 0.203, 0.203, 0.152, 0.152, 0.152, 
                   0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152)

# Calculate transition probabilities using the given formula
calc_transition_prob <- function(y, t) {
  1 - exp(log(y) / t)
}

num_cycles <- length(OS_long_term)
transition_probs_long_term <- data.frame(
  cycle = 1:num_cycles,
  prob_stable_to_death = sapply(1:num_cycles, function(t) calc_transition_prob(OS_long_term[t], t)),
  prob_stable_to_progressive = sapply(1:num_cycles, function(t) calc_transition_prob(PFS_long_term[t], t)),
  prob_progressive_to_death = sapply(1:num_cycles, function(t) calc_transition_prob(OS_long_term[t] - PFS_long_term[t], t))
)

# Calculate probabilities for remaining states
transition_probs_long_term$prob_stable_to_stable <- 1 - (transition_probs_long_term$prob_stable_to_death + transition_probs_long_term$prob_stable_to_progressive)
transition_probs_long_term$prob_progressive_to_progressive <- 1 - transition_probs_long_term$prob_progressive_to_death

# Store transition probabilities without printing

# Define function to calculate Markov trace
calculate_markov_trace <- function(transition_probs, num_cycles) {
  # Initialize Markov trace
  markov_trace <- data.frame(
    cycle = 1:num_cycles,
    stable = numeric(num_cycles),
    progressive = numeric(num_cycles),
    death = numeric(num_cycles)
  )
  
  # Initial state distribution
  markov_trace$stable[1] <- 1
  markov_trace$progressive[1] <- 0
  markov_trace$death[1] <- 0
  
  # Apply transition probabilities from cycle t-2 (with special rule for cycles 2 and 3)
  for (t in 2:num_cycles) {
    trans_cycle <- ifelse(t <= 3, 1, t - 2)
    
    markov_trace$stable[t] <- markov_trace$stable[t - 1] * transition_probs$prob_stable_to_stable[trans_cycle]
    markov_trace$progressive[t] <- markov_trace$progressive[t - 1] * transition_probs$prob_progressive_to_progressive[trans_cycle] +
      markov_trace$stable[t - 1] * transition_probs$prob_stable_to_progressive[trans_cycle]
    markov_trace$death[t] <- 1 - (markov_trace$stable[t] + markov_trace$progressive[t])
  }
  
  return(markov_trace)
}

# Number of cycles
num_cycles <- 60

# Calculate Markov traces for both Standard and Long-term TMZ therapy
markov_trace_standard <- calculate_markov_trace(transition_probs_standard, num_cycles)
markov_trace_long_term <- calculate_markov_trace(transition_probs_long_term, num_cycles)

# Store Markov traces without printing

# Define function to calculate QALMs
calculate_qalms <- function(markov_trace, health_utilities, num_cycles) {
  qalms <- numeric(num_cycles)
  
  # Initialize progressive state utility values
  progressive_utilities <- rep(health_utilities$progressive, num_cycles)
  
  # Apply decrement for progressive state from cycle 3 to cycle 25
  for (t in 3:25) {
    progressive_utilities[t] <- max(health_utilities$progressive - (0.02 * (t - 2)), 0) # Ensure utility is non-negative
  }
  
  # Fix the utility at cycle 25 value for the remaining cycles
  progressive_utilities[26:num_cycles] <- progressive_utilities[25]
  
  # Calculate QALMs per cycle
  for (t in 1:num_cycles) {
    qalms[t] <- (markov_trace$stable[t] * health_utilities$stable) +
      (markov_trace$progressive[t] * progressive_utilities[t])
  }
  
  # Return results
  return(data.frame(cycle = 1:num_cycles, qalms = qalms))
}

# Number of cycles
num_cycles <- 60

# Define health utilities
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731
)

# Calculate QALMs for both Standard and Long-term TMZ therapy
qalms_standard <- calculate_qalms(markov_trace_standard, health_utilities, num_cycles)
qalms_long_term <- calculate_qalms(markov_trace_long_term, health_utilities, num_cycles)

# Calculate total QALYs by summing QALMs and dividing by 12
total_qalys_standard <- sum(qalms_standard$qalms) / 12
total_qalys_long_term <- sum(qalms_long_term$qalms) / 12

# Store QALYs without printing

# Define function to calculate costs per cycle
calculate_costs <- function(markov_trace, costs, num_cycles) {
  cost_per_cycle <- numeric(num_cycles)
  
  # Assign cost values based on cycle range
  cost_stable <- c(rep(costs$stable_1_3, 4), rep(costs$stable_4_6, 3), rep(costs$stable_subsequent, 53))
  cost_progressive <- rep(costs$progressive, num_cycles)
  cost_death <- rep(costs$death, num_cycles)
  
  # Calculate costs per cycle
  for (t in 1:num_cycles) {
    cost_per_cycle[t] <- (markov_trace$stable[t] * cost_stable[t]) +
      (markov_trace$progressive[t] * cost_progressive[t]) +
      (markov_trace$death[t] * cost_death[t])
  }
  
  # Return results
  return(data.frame(cycle = 1:num_cycles, cost_per_cycle = cost_per_cycle))
}

# Number of cycles
num_cycles <- 60

# Define cost structures
costs_standard <- list(
  stable_1_3 = 1944,
  stable_4_6 = 2453,
  stable_subsequent = 350,
  progressive = 2720,
  death = 0
)

costs_long_term <- list(
  stable_1_3 = 1944,
  stable_4_6 = 2453,
  stable_subsequent = 2453,
  progressive = 2720,
  death = 0
)

# Calculate costs for both Standard and Long-term TMZ therapy
costs_standard_trace <- calculate_costs(markov_trace_standard, costs_standard, num_cycles)
costs_long_term_trace <- calculate_costs(markov_trace_long_term, costs_long_term, num_cycles)

# Calculate total costs
total_cost_standard <- sum(costs_standard_trace$cost_per_cycle)
total_cost_long_term <- sum(costs_long_term_trace$cost_per_cycle)

# Store costs without printing

# Calculate Incremental Cost-Effectiveness Ratio (ICER)
calculate_icer <- function(total_costs_treatment, total_costs_control, total_effects_treatment, total_effects_control) {
  icer <- (total_costs_treatment - total_costs_control) / (total_effects_treatment - total_effects_control)
  return(round(icer, 0))  # Round to the nearest whole number
}

# Calculate ICER for QALMs
icer_qalms <- calculate_icer(total_cost_long_term, total_cost_standard, sum(qalms_long_term$qalms), sum(qalms_standard$qalms))

# Calculate ICER for QALYs
icer_qalys <- calculate_icer(total_cost_long_term, total_cost_standard, total_qalys_long_term, total_qalys_standard)

# Display results
list(
  ICER_QALMs = icer_qalms,
  ICER_QALYs = icer_qalys
)
