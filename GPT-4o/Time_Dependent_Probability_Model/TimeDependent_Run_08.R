# Store input data without printing

# Health utility data
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Costs data for Standard TMZ therapy
costs_standard_tmz <- list(
  stable_months_1_3 = 1944,
  stable_months_4_6 = 2453,
  stable_subsequent = 350,
  progressive = 2720,
  death = 0
)

# Costs data for Long-term TMZ therapy
costs_long_term_tmz <- list(
  stable_months_1_3 = 1944,
  stable_months_4_6 = 2453,
  stable_subsequent = 2453,
  progressive = 2720,
  death = 0
)

# Cycle length and time horizon
cycle_length <- 1  # months
time_horizon <- 60 # months

# Define survival data for Standard TMZ therapy
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

# Function to calculate transition probabilities
calculate_transition_probability <- function(y, t) {
  return(1 - exp(log(y)/t))
}

# Number of cycles
num_cycles <- length(OS_standard)

# Calculate transition probabilities for each cycle
transition_probs_standard <- data.frame(
  cycle = 1:num_cycles,
  stable_to_death = sapply(1:num_cycles, function(t) calculate_transition_probability(OS_standard[t], t)),
  stable_to_progressive = sapply(1:num_cycles, function(t) calculate_transition_probability(PFS_standard[t], t)),
  progressive_to_death = sapply(1:num_cycles, function(t) calculate_transition_probability(OS_standard[t] - PFS_standard[t], t))
)

# Calculate transition probabilities stable to stable and progressive to progressive
transition_probs_standard$stable_to_stable <- 1 - (transition_probs_standard$stable_to_progressive + transition_probs_standard$stable_to_death)
transition_probs_standard$progressive_to_progressive <- 1 - transition_probs_standard$progressive_to_death

# Store the transition probabilities without printing
transition_probs_standard

# Define survival data for Long-term TMZ therapy
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

# Function to calculate transition probabilities
calculate_transition_probability <- function(y, t) {
  return(1 - exp(log(y)/t))
}

# Number of cycles
num_cycles <- length(OS_long_term)

# Calculate transition probabilities for each cycle
transition_probs_long_term <- data.frame(
  cycle = 1:num_cycles,
  stable_to_death = sapply(1:num_cycles, function(t) calculate_transition_probability(OS_long_term[t], t)),
  stable_to_progressive = sapply(1:num_cycles, function(t) calculate_transition_probability(PFS_long_term[t], t)),
  progressive_to_death = sapply(1:num_cycles, function(t) calculate_transition_probability(OS_long_term[t] - PFS_long_term[t], t))
)

# Calculate transition probabilities stable to stable and progressive to progressive
transition_probs_long_term$stable_to_stable <- 1 - (transition_probs_long_term$stable_to_progressive + transition_probs_long_term$stable_to_death)
transition_probs_long_term$progressive_to_progressive <- 1 - transition_probs_long_term$progressive_to_death

# Store the transition probabilities without printing
transition_probs_long_term

# Function to compute Markov trace over 60 cycles
compute_markov_trace <- function(transition_probs) {
  num_cycles <- 60
  markov_trace <- matrix(0, nrow = num_cycles, ncol = 3)
  colnames(markov_trace) <- c("Stable", "Progressive", "Dead")
  
  # Initial state distribution
  markov_trace[1, ] <- c(1, 0, 0)  # All patients start in the stable state
  
  # Compute state transitions based on given rules
  for (t in 2:num_cycles) {
    transition_cycle <- ifelse(t <= 3, 1, t - 2)
    
    stable_to_stable <- transition_probs$stable_to_stable[transition_cycle]
    stable_to_progressive <- transition_probs$stable_to_progressive[transition_cycle]
    stable_to_death <- transition_probs$stable_to_death[transition_cycle]
    progressive_to_progressive <- transition_probs$progressive_to_progressive[transition_cycle]
    progressive_to_death <- transition_probs$progressive_to_death[transition_cycle]
    
    # Update states
    markov_trace[t, "Stable"] <- markov_trace[t - 1, "Stable"] * stable_to_stable
    markov_trace[t, "Progressive"] <- markov_trace[t - 1, "Stable"] * stable_to_progressive + markov_trace[t - 1, "Progressive"] * progressive_to_progressive
    markov_trace[t, "Dead"] <- markov_trace[t - 1, "Stable"] * stable_to_death + markov_trace[t - 1, "Progressive"] * progressive_to_death + markov_trace[t - 1, "Dead"]
  }
  
  return(as.data.frame(markov_trace))
}

# Compute Markov traces for both treatments
markov_trace_standard <- compute_markov_trace(transition_probs_standard)
markov_trace_long_term <- compute_markov_trace(transition_probs_long_term)

# Store the results without printing
markov_trace_standard
markov_trace_long_term

# Define health utilities
utility_stable <- 0.743
utility_progressive_initial <- 0.731
utility_decrement_per_month <- 0.02
max_decrement_cycles <- 23

# Function to compute QALMs
compute_qalms <- function(markov_trace) {
  num_cycles <- nrow(markov_trace)
  qalms <- numeric(num_cycles)
  
  # Initialize progressive utility values
  utility_progressive <- rep(utility_progressive_initial, num_cycles)
  
  # Apply utility decrement for the defined range (cycles 3 to 25)
  for (t in 3:(2 + max_decrement_cycles)) {
    utility_progressive[t] <- utility_progressive[t - 1] - utility_decrement_per_month
  }
  
  # Fix the progressive utility value from cycle 26 onwards
  utility_progressive[(2 + max_decrement_cycles + 1):num_cycles] <- utility_progressive[2 + max_decrement_cycles]
  
  # Calculate QALMs for each cycle
  for (t in 1:num_cycles) {
    qalms[t] <- markov_trace[t, "Stable"] * utility_stable + 
      markov_trace[t, "Progressive"] * utility_progressive[t]
  }
  
  return(qalms)
}

# Compute QALMs for both treatments
qalms_standard <- compute_qalms(markov_trace_standard)
qalms_long_term <- compute_qalms(markov_trace_long_term)

# Compute total QALMs by summing all cycles
total_qalms_standard <- sum(qalms_standard)
total_qalms_long_term <- sum(qalms_long_term)

# Convert total QALMs to total QALYs (divide by 12 to convert months to years)
total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Store results without printing
qalms_standard
qalms_long_term
total_qalms_standard
total_qalms_long_term
total_qalys_standard
total_qalys_long_term

# Define cost data
costs_standard_tmz <- c(1944, 1944, 1944, 1944, 2453, 2453, 2453, rep(350, 53))
costs_long_term_tmz <- c(1944, 1944, 1944, 1944, 2453, 2453, 2453, rep(2453, 53))
costs_progressive <- 2720
costs_death <- 0

# Function to compute costs
compute_costs <- function(markov_trace, stable_costs) {
  num_cycles <- nrow(markov_trace)
  costs <- numeric(num_cycles)
  
  # Calculate costs for each cycle
  for (t in 1:num_cycles) {
    costs[t] <- (markov_trace[t, "Stable"] * stable_costs[t]) + 
      (markov_trace[t, "Progressive"] * costs_progressive) + 
      (markov_trace[t, "Dead"] * costs_death)
  }
  
  return(costs)
}

# Compute costs for both treatments
costs_per_cycle_standard <- compute_costs(markov_trace_standard, costs_standard_tmz)
costs_per_cycle_long_term <- compute_costs(markov_trace_long_term, costs_long_term_tmz)

# Compute total costs by summing all cycles
total_costs_standard <- sum(costs_per_cycle_standard)
total_costs_long_term <- sum(costs_per_cycle_long_term)

# Store results without printing
costs_per_cycle_standard
costs_per_cycle_long_term
total_costs_standard
total_costs_long_term

# Compute incremental cost-effectiveness ratio (ICER)
compute_icer <- function(costs_new, costs_old, qalms_new, qalms_old, qalys_new, qalys_old) {
  icer_qalms <- (costs_new - costs_old) / (qalms_new - qalms_old)
  icer_qalys <- (costs_new - costs_old) / (qalys_new - qalys_old)
  
  # Round ICER values to whole numbers
  icer_qalms <- round(icer_qalms)
  icer_qalys <- round(icer_qalys)
  
  return(list(icer_qalms = icer_qalms, icer_qalys = icer_qalys))
}

# Calculate ICER for Long-term TMZ compared to Standard TMZ
icer_results <- compute_icer(
  costs_new = total_costs_long_term,
  costs_old = total_costs_standard,
  qalms_new = total_qalms_long_term,
  qalms_old = total_qalms_standard,
  qalys_new = total_qalys_long_term,
  qalys_old = total_qalys_standard
)

# Store results without printing
icer_qalms <- icer_results$icer_qalms
icer_qalys <- icer_results$icer_qalys
