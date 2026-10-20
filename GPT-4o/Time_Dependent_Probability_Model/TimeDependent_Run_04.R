# Define health utility data
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Define cost data for Standard TMZ therapy
costs_standard <- list(
  stable_1_3 = 1944,
  stable_4_6 = 2453,
  stable_subsequent = 350,
  progressive = 2720,
  death = 0
)

# Define cost data for Long-term TMZ therapy
costs_long_term <- list(
  stable_1_3 = 1944,
  stable_4_6 = 2453,
  stable_subsequent = 2453,
  progressive = 2720,
  death = 0
)

# Define cycle length and time horizon
cycle_length <- 1  # in months
time_horizon <- 60 # in months

# Define Overall Survival (OS) and Progression-Free Survival (PFS) for Standard TMZ therapy
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

# Number of cycles
n_cycles <- length(OS_standard)

# Initialize transition probability storage
transition_probs_standard <- data.frame(
  cycle = 1:n_cycles,
  p_stable_to_progressive = rep(NA, n_cycles),
  p_stable_to_death = rep(NA, n_cycles),
  p_progressive_to_death = rep(NA, n_cycles)
)

# Calculate transition probabilities
for (t in 1:n_cycles) {
  transition_probs_standard$p_stable_to_progressive[t] <- 1 - exp(log(PFS_standard[t]) / t)
  transition_probs_standard$p_stable_to_death[t] <- 1 - exp(log(OS_standard[t]) / t)
  transition_probs_standard$p_progressive_to_death[t] <- 1 - exp(log(OS_standard[t] - PFS_standard[t]) / t)
}

# Store the transition probabilities without printing them
transition_probs_standard

# Calculate transition probabilities for stable to stable
transition_probs_standard$p_stable_to_stable <- 1 - transition_probs_standard$p_stable_to_progressive - transition_probs_standard$p_stable_to_death

# Calculate transition probabilities for progressive to progressive
transition_probs_standard$p_progressive_to_progressive <- 1 - transition_probs_standard$p_progressive_to_death

# Store the updated transition probabilities without printing them
transition_probs_standard

# Define Overall Survival (OS) and Progression-Free Survival (PFS) for Long-term TMZ therapy
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

# Number of cycles
n_cycles <- length(OS_long_term)

# Initialize transition probability storage
transition_probs_long_term <- data.frame(
  cycle = 1:n_cycles,
  p_stable_to_progressive = rep(NA, n_cycles),
  p_stable_to_death = rep(NA, n_cycles),
  p_progressive_to_death = rep(NA, n_cycles)
)

# Calculate transition probabilities
for (t in 1:n_cycles) {
  transition_probs_long_term$p_stable_to_progressive[t] <- 1 - exp(log(PFS_long_term[t]) / t)
  transition_probs_long_term$p_stable_to_death[t] <- 1 - exp(log(OS_long_term[t]) / t)
  transition_probs_long_term$p_progressive_to_death[t] <- 1 - exp(log(OS_long_term[t] - PFS_long_term[t]) / t)
}

# Store the transition probabilities without printing them
transition_probs_long_term

# Calculate transition probabilities for stable to stable
transition_probs_long_term$p_stable_to_stable <- 1 - transition_probs_long_term$p_stable_to_progressive - transition_probs_long_term$p_stable_to_death

# Calculate transition probabilities for progressive to progressive
transition_probs_long_term$p_progressive_to_progressive <- 1 - transition_probs_long_term$p_progressive_to_death

# Store the updated transition probabilities without printing them
transition_probs_long_term

# Number of cycles
n_cycles <- 60

# Initialize Markov trace for Standard TMZ therapy
markov_trace_standard <- data.frame(
  cycle = 1:n_cycles,
  stable = rep(NA, n_cycles),
  progressive = rep(NA, n_cycles),
  death = rep(NA, n_cycles)
)

# Initialize Markov trace for Long-term TMZ therapy
markov_trace_long_term <- data.frame(
  cycle = 1:n_cycles,
  stable = rep(NA, n_cycles),
  progressive = rep(NA, n_cycles),
  death = rep(NA, n_cycles)
)

# Initial state distribution (all patients start in stable state)
markov_trace_standard$stable[1] <- 1
markov_trace_standard$progressive[1] <- 0
markov_trace_standard$death[1] <- 0

markov_trace_long_term$stable[1] <- 1
markov_trace_long_term$progressive[1] <- 0
markov_trace_long_term$death[1] <- 0

# Markov trace calculations
for (t in 2:n_cycles) {
  # Determine the transition probabilities for the cycle based on the specified rule
  if (t == 2 || t == 3) {
    tp_standard <- transition_probs_standard[1, ]
    tp_long_term <- transition_probs_long_term[1, ]
  } else {
    tp_standard <- transition_probs_standard[t - 2, ]
    tp_long_term <- transition_probs_long_term[t - 2, ]
  }
  
  # Standard TMZ therapy
  markov_trace_standard$stable[t] <- markov_trace_standard$stable[t - 1] * tp_standard$p_stable_to_stable[t - 1]
  markov_trace_standard$progressive[t] <- markov_trace_standard$progressive[t - 1] * tp_standard$p_progressive_to_progressive[t - 1] +
    markov_trace_standard$stable[t - 1] * tp_standard$p_stable_to_progressive[t - 1]
  markov_trace_standard$death[t] <- 1 - (markov_trace_standard$stable[t] + markov_trace_standard$progressive[t])
  
  # Long-term TMZ therapy
  markov_trace_long_term$stable[t] <- markov_trace_long_term$stable[t - 1] * tp_long_term$p_stable_to_stable[t - 1]
  markov_trace_long_term$progressive[t] <- markov_trace_long_term$progressive[t - 1] * tp_long_term$p_progressive_to_progressive[t - 1] +
    markov_trace_long_term$stable[t - 1] * tp_long_term$p_stable_to_progressive[t - 1]
  markov_trace_long_term$death[t] <- 1 - (markov_trace_long_term$stable[t] + markov_trace_long_term$progressive[t])
}

# Store the Markov traces without printing them
markov_trace_standard
markov_trace_long_term

# Initialize QALMs storage
qalms_standard <- data.frame(
  cycle = 1:n_cycles,
  qalms = rep(NA, n_cycles)
)

qalms_long_term <- data.frame(
  cycle = 1:n_cycles,
  qalms = rep(NA, n_cycles)
)

# Define health state utilities
utility_stable <- 0.743
utility_progressive <- 0.731
utility_decrement <- 0.02
decrement_start_cycle <- 3  # Decrement starts at cycle 3
decrement_end_cycle <- 25   # Decrement continues until cycle 25
max_decrements <- decrement_end_cycle - decrement_start_cycle + 1

# Generate adjusted utility values for the progressive state
progressive_utilities <- rep(utility_progressive, n_cycles)

for (t in decrement_start_cycle:decrement_end_cycle) {
  decrement_amount <- (t - decrement_start_cycle + 1) * utility_decrement
  progressive_utilities[t] <- max(utility_progressive - decrement_amount, 0)
}

# After cycle 25, maintain the last decremented value
progressive_utilities[(decrement_end_cycle + 1):n_cycles] <- progressive_utilities[decrement_end_cycle]

# Calculate QALMs for each cycle
for (t in 1:n_cycles) {
  qalms_standard$qalms[t] <- markov_trace_standard$stable[t] * utility_stable +
    markov_trace_standard$progressive[t] * progressive_utilities[t]
  
  qalms_long_term$qalms[t] <- markov_trace_long_term$stable[t] * utility_stable +
    markov_trace_long_term$progressive[t] * progressive_utilities[t]
}

# Calculate total QALMs
total_qalms_standard <- sum(qalms_standard$qalms)
total_qalms_long_term <- sum(qalms_long_term$qalms)

# Store QALMs results without printing them
qalms_standard
qalms_long_term
total_qalms_standard
total_qalms_long_term

# Convert total QALMs to total QALYs
total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Store the total QALYs without printing them
total_qalys_standard
total_qalys_long_term

# Initialize cost storage
costs_standard_trace <- data.frame(
  cycle = 1:n_cycles,
  cost = rep(NA, n_cycles)
)

costs_long_term_trace <- data.frame(
  cycle = 1:n_cycles,
  cost = rep(NA, n_cycles)
)

# Define cost application rules based on cycles
get_cost_standard <- function(cycle) {
  if (cycle <= 4) {
    return(costs_standard$stable_1_3)
  } else if (cycle <= 7) {
    return(costs_standard$stable_4_6)
  } else {
    return(costs_standard$stable_subsequent)
  }
}

get_cost_long_term <- function(cycle) {
  if (cycle <= 4) {
    return(costs_long_term$stable_1_3)
  } else if (cycle <= 7) {
    return(costs_long_term$stable_4_6)
  } else {
    return(costs_long_term$stable_subsequent)
  }
}

# Calculate costs for each cycle
for (t in 1:n_cycles) {
  # Get cost per cycle for stable state
  cost_standard_stable <- get_cost_standard(t)
  cost_long_term_stable <- get_cost_long_term(t)
  
  # Compute total cost for each cycle based on state proportions
  costs_standard_trace$cost[t] <- markov_trace_standard$stable[t] * cost_standard_stable +
    markov_trace_standard$progressive[t] * costs_standard$progressive +
    markov_trace_standard$death[t] * costs_standard$death
  
  costs_long_term_trace$cost[t] <- markov_trace_long_term$stable[t] * cost_long_term_stable +
    markov_trace_long_term$progressive[t] * costs_long_term$progressive +
    markov_trace_long_term$death[t] * costs_long_term$death
}

# Calculate total costs
total_cost_standard <- sum(costs_standard_trace$cost)
total_cost_long_term <- sum(costs_long_term_trace$cost)

# Store cost results without printing them
costs_standard_trace
costs_long_term_trace
total_cost_standard
total_cost_long_term

# Calculate the incremental differences
incremental_cost <- total_cost_long_term - total_cost_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- total_qalys_long_term - total_qalys_standard

# Calculate ICER for QALMs and QALYs
icer_qalms <- round(incremental_cost / incremental_qalms, 0)
icer_qalys <- round(incremental_cost / incremental_qalys, 0)

# Store ICER results without printing them
icer_qalms
icer_qalys
