# Define health state utilities
health_utility <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Define cost data
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

# Define model parameters
cycle_length <- 1  # 1 month
horizon <- 60       # 60 months

# Define OS and PFS data
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
time_horizon <- length(OS_standard)

# Calculate transition probabilities
stable_to_death <- 1 - exp(log(OS_standard) / (1:time_horizon))
stable_to_progressive <- 1 - exp(log(PFS_standard) / (1:time_horizon))
progressive_to_death <- 1 - exp(log(OS_standard - PFS_standard) / (1:time_horizon))

# Calculate stable to stable and progressive to progressive
stable_to_stable <- 1 - (stable_to_progressive + stable_to_death)
progressive_to_progressive <- 1 - progressive_to_death

# Store transition probabilities
transition_probs_standard <- data.frame(
  cycle = 1:time_horizon,
  stable_to_death = stable_to_death,
  stable_to_progressive = stable_to_progressive,
  progressive_to_death = progressive_to_death,
  stable_to_stable = stable_to_stable,
  progressive_to_progressive = progressive_to_progressive
)

# Store transition probabilities
transition_probs_standard

# Define OS and PFS data for long-term TMZ therapy
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
time_horizon <- length(OS_long_term)

# Calculate transition probabilities
stable_to_death <- 1 - exp(log(OS_long_term) / (1:time_horizon))
stable_to_progressive <- 1 - exp(log(PFS_long_term) / (1:time_horizon))
progressive_to_death <- 1 - exp(log(OS_long_term - PFS_long_term) / (1:time_horizon))

# Calculate stable to stable and progressive to progressive
stable_to_stable <- 1 - (stable_to_progressive + stable_to_death)
progressive_to_progressive <- 1 - progressive_to_death

# Store transition probabilities
transition_probs_long_term <- data.frame(
  cycle = 1:time_horizon,
  stable_to_death = stable_to_death,
  stable_to_progressive = stable_to_progressive,
  progressive_to_death = progressive_to_death,
  stable_to_stable = stable_to_stable,
  progressive_to_progressive = progressive_to_progressive
)

# Store transition probabilities
transition_probs_long_term

# Initialize Markov trace for 60 cycles
num_cycles <- 60

# Initial state distribution: 100% in stable state
markov_trace_standard <- data.frame(cycle = 1:num_cycles, stable = 0, progressive = 0, death = 0)
markov_trace_long_term <- data.frame(cycle = 1:num_cycles, stable = 0, progressive = 0, death = 0)

markov_trace_standard$stable[1] <- 1
markov_trace_long_term$stable[1] <- 1

# Iterate through cycles to compute state distributions
for (t in 2:num_cycles) {
  if (t == 2 || t == 3) {
    trans_prob_standard <- transition_probs_standard[1, ]
    trans_prob_long_term <- transition_probs_long_term[1, ]
  } else {
    trans_prob_standard <- transition_probs_standard[t - 2, ]
    trans_prob_long_term <- transition_probs_long_term[t - 2, ]
  }
  
  # Update Standard TMZ therapy state proportions
  markov_trace_standard$stable[t] <- markov_trace_standard$stable[t-1] * trans_prob_standard$stable_to_stable
  markov_trace_standard$progressive[t] <- (markov_trace_standard$stable[t-1] * trans_prob_standard$stable_to_progressive) + 
    (markov_trace_standard$progressive[t-1] * trans_prob_standard$progressive_to_progressive)
  markov_trace_standard$death[t] <- 1 - (markov_trace_standard$stable[t] + markov_trace_standard$progressive[t])
  
  # Update Long-term TMZ therapy state proportions
  markov_trace_long_term$stable[t] <- markov_trace_long_term$stable[t-1] * trans_prob_long_term$stable_to_stable
  markov_trace_long_term$progressive[t] <- (markov_trace_long_term$stable[t-1] * trans_prob_long_term$stable_to_progressive) + 
    (markov_trace_long_term$progressive[t-1] * trans_prob_long_term$progressive_to_progressive)
  markov_trace_long_term$death[t] <- 1 - (markov_trace_long_term$stable[t] + markov_trace_long_term$progressive[t])
}

# Store Markov traces
markov_trace_standard
markov_trace_long_term

# Define health state utilities
utility_stable <- 0.743
utility_progressive <- 0.731
utility_decrement <- 0.02

# Initialize QALMs storage
qalms_standard <- data.frame(cycle = 1:num_cycles, qalms = 0)
qalms_long_term <- data.frame(cycle = 1:num_cycles, qalms = 0)

# Adjust progressive utility over time with decrement
progressive_utilities <- rep(utility_progressive, num_cycles)
for (t in 3:25) {  # Apply decrement from cycle 3 to 25
  progressive_utilities[t] <- max(utility_progressive - ((t - 2) * utility_decrement), 0)  # Ensure utility doesn't go below 0
}
progressive_utilities[26:num_cycles] <- progressive_utilities[25]  # Keep fixed after cycle 25

# Calculate QALMs for each cycle
for (t in 1:num_cycles) {
  qalms_standard$qalms[t] <- (markov_trace_standard$stable[t] * utility_stable) + 
    (markov_trace_standard$progressive[t] * progressive_utilities[t])
  
  qalms_long_term$qalms[t] <- (markov_trace_long_term$stable[t] * utility_stable) + 
    (markov_trace_long_term$progressive[t] * progressive_utilities[t])
}

# Calculate total QALMs
total_qalms_standard <- sum(qalms_standard$qalms)
total_qalms_long_term <- sum(qalms_long_term$qalms)

# Convert QALMs to QALYs (since 1 QALY = 12 QALMs)
total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Store results
qalms_standard
total_qalms_standard
total_qalys_standard
qalms_long_term
total_qalms_long_term
total_qalys_long_term

# Define cost categories for Standard TMZ therapy
costs_standard <- c(rep(1944, 4), rep(2453, 3), rep(350, 53))
costs_long_term <- c(rep(1944, 4), rep(2453, 3), rep(2453, 53))

# Initialize cost storage
total_costs_standard <- data.frame(cycle = 1:num_cycles, cost = 0)
total_costs_long_term <- data.frame(cycle = 1:num_cycles, cost = 0)

# Calculate costs for each cycle
for (t in 1:num_cycles) {
  total_costs_standard$cost[t] <- (markov_trace_standard$stable[t] * costs_standard[t]) + 
    (markov_trace_standard$progressive[t] * 2720) + 
    (markov_trace_standard$death[t] * 0)
  
  total_costs_long_term$cost[t] <- (markov_trace_long_term$stable[t] * costs_long_term[t]) + 
    (markov_trace_long_term$progressive[t] * 2720) + 
    (markov_trace_long_term$death[t] * 0)
}

# Calculate total costs
total_cost_standard <- sum(total_costs_standard$cost)
total_cost_long_term <- sum(total_costs_long_term$cost)

# Store results
total_costs_standard
total_cost_standard
total_costs_long_term
total_cost_long_term

# Calculate incremental cost-effectiveness ratio (ICER) for QALMs and QALYs

# Incremental costs
incremental_cost <- total_cost_long_term - total_cost_standard

# Incremental QALMs
incremental_qalms <- total_qalms_long_term - total_qalms_standard

# Incremental QALYs
incremental_qalys <- total_qalys_long_term - total_qalys_standard

# Calculate ICERs
icer_qalms <- round(incremental_cost / incremental_qalms, 0)
nicer_qalys <- round(incremental_cost / incremental_qalys, 0)

# Store results
icer_qalms
nicer_qalys
