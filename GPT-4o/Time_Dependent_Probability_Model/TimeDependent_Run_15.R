# Store health utility data
health_utility <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Store cost data for Standard TMZ therapy
cost_standard <- list(
  stable_1_3 = 1944,
  stable_4_6 = 2453,
  stable_subsequent = 350,
  progressive = 2720,
  death = 0
)

# Store cost data for Long-term TMZ therapy
cost_long_term <- list(
  stable_1_3 = 1944,
  stable_4_6 = 2453,
  stable_subsequent = 2453,
  progressive = 2720,
  death = 0
)

# Define cycle length and time horizon
cycle_length <- 1  # in months
time_horizon <- 60 # in months

# Define the given OS and PFS values for Standard TMZ therapy
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

# Calculate transition probabilities
transition_probs_standard <- data.frame(
  cycle = 1:60,
  stable_to_death = 1 - exp(log(OS_standard) / (1:60)),
  stable_to_progressive = 1 - exp(log(PFS_standard) / (1:60)),
  progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / (1:60))
)

# Calculate transition probabilities for stable to stable and progressive to progressive
transition_probs_standard$stable_to_stable <- 1 - transition_probs_standard$stable_to_death - transition_probs_standard$stable_to_progressive
transition_probs_standard$progressive_to_progressive <- 1 - transition_probs_standard$progressive_to_death

# Store the transition probabilities without printing them

# Define the given OS and PFS values for Long-term TMZ therapy
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

# Calculate transition probabilities
transition_probs_long_term <- data.frame(
  cycle = 1:60,
  stable_to_death = 1 - exp(log(OS_long_term) / (1:60)),
  stable_to_progressive = 1 - exp(log(PFS_long_term) / (1:60)),
  progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / (1:60))
)

# Calculate transition probabilities for stable to stable and progressive to progressive
transition_probs_long_term$stable_to_stable <- 1 - transition_probs_long_term$stable_to_death - transition_probs_long_term$stable_to_progressive
transition_probs_long_term$progressive_to_progressive <- 1 - transition_probs_long_term$progressive_to_death

# Store the transition probabilities without printing them

# Define the number of cycles
num_cycles <- 60

# Initialize the Markov trace for Standard TMZ therapy
trace_standard <- data.frame(
  cycle = 1:num_cycles,
  stable = numeric(num_cycles),
  progressive = numeric(num_cycles),
  death = numeric(num_cycles)
)

# Initialize the Markov trace for Long-term TMZ therapy
trace_long_term <- data.frame(
  cycle = 1:num_cycles,
  stable = numeric(num_cycles),
  progressive = numeric(num_cycles),
  death = numeric(num_cycles)
)

# Set initial state distribution (Cycle 1)
trace_standard$stable[1] <- 1
trace_standard$progressive[1] <- 0
trace_standard$death[1] <- 0

trace_long_term$stable[1] <- 1
trace_long_term$progressive[1] <- 0
trace_long_term$death[1] <- 0

# Compute the Markov trace using specified transition rules
for (t in 2:num_cycles) {
  # Use transition probabilities from two cycles earlier, except for cycles 2 and 3
  trans_index <- ifelse(t <= 3, 1, t - 2)
  
  # Standard TMZ Therapy
  trace_standard$stable[t] <- trace_standard$stable[t - 1] * transition_probs_standard$stable_to_stable[trans_index]
  trace_standard$progressive[t] <- (trace_standard$stable[t - 1] * transition_probs_standard$stable_to_progressive[trans_index]) +
    (trace_standard$progressive[t - 1] * transition_probs_standard$progressive_to_progressive[trans_index])
  trace_standard$death[t] <- 1 - (trace_standard$stable[t] + trace_standard$progressive[t])
  
  # Long-term TMZ Therapy
  trace_long_term$stable[t] <- trace_long_term$stable[t - 1] * transition_probs_long_term$stable_to_stable[trans_index]
  trace_long_term$progressive[t] <- (trace_long_term$stable[t - 1] * transition_probs_long_term$stable_to_progressive[trans_index]) +
    (trace_long_term$progressive[t - 1] * transition_probs_long_term$progressive_to_progressive[trans_index])
  trace_long_term$death[t] <- 1 - (trace_long_term$stable[t] + trace_long_term$progressive[t])
}

# Store the Markov traces without printing them

# Define health utility values
utility_stable <- 0.743
utility_progressive <- 0.731
utility_decrement <- 0.02

decremented_utilities <- numeric(num_cycles)

decremented_utilities[1:2] <- utility_progressive  # First two cycles use original utility
for (t in 3:25) {
  decremented_utilities[t] <- max(utility_progressive - ((t - 2) * utility_decrement), 0)
}
decremented_utilities[26:num_cycles] <- decremented_utilities[25]  # Keep fixed after cycle 25

# Initialize QALMs calculations for both treatments
qalm_standard <- numeric(num_cycles)
qalm_long_term <- numeric(num_cycles)

# Compute QALMs for each cycle
for (t in 1:num_cycles) {
  qalm_standard[t] <- (trace_standard$stable[t] * utility_stable) +
    (trace_standard$progressive[t] * decremented_utilities[t])
  
  qalm_long_term[t] <- (trace_long_term$stable[t] * utility_stable) +
    (trace_long_term$progressive[t] * decremented_utilities[t])
}

# Compute total QALMs
total_qalm_standard <- sum(qalm_standard)
total_qalm_long_term <- sum(qalm_long_term)

# Compute total QALYs
total_qaly_standard <- total_qalm_standard / 12
total_qaly_long_term <- total_qalm_long_term / 12

# Store QALMs and QALYs without printing them

# Define cost values for Standard TMZ therapy
cost_standard_stable <- c(rep(1944, 4), rep(2453, 3), rep(350, 53))
cost_standard_progressive <- rep(2720, num_cycles)
cost_standard_death <- rep(0, num_cycles)

# Define cost values for Long-term TMZ therapy
cost_long_term_stable <- c(rep(1944, 4), rep(2453, 3), rep(2453, 53))
cost_long_term_progressive <- rep(2720, num_cycles)
cost_long_term_death <- rep(0, num_cycles)

# Initialize cost calculations for both treatments
cost_standard <- numeric(num_cycles)
cost_long_term <- numeric(num_cycles)

# Compute costs for each cycle
for (t in 1:num_cycles) {
  cost_standard[t] <- (trace_standard$stable[t] * cost_standard_stable[t]) +
    (trace_standard$progressive[t] * cost_standard_progressive[t]) +
    (trace_standard$death[t] * cost_standard_death[t])
  
  cost_long_term[t] <- (trace_long_term$stable[t] * cost_long_term_stable[t]) +
    (trace_long_term$progressive[t] * cost_long_term_progressive[t]) +
    (trace_long_term$death[t] * cost_long_term_death[t])
}

# Compute total costs
total_cost_standard <- sum(cost_standard)
total_cost_long_term <- sum(cost_long_term)

# Store costs without printing them

# Calculate incremental values
incremental_cost <- total_cost_long_term - total_cost_standard
incremental_qalm <- total_qalm_long_term - total_qalm_standard
incremental_qaly <- total_qaly_long_term - total_qaly_standard

# Calculate ICER for QALMs and QALYs
icer_qalm <- round(incremental_cost / incremental_qalm, 0)
icer_qaly <- round(incremental_cost / incremental_qaly, 0)

# Store ICER values without printing them
