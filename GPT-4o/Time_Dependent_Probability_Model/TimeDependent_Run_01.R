# Store health utility data
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Store cost data
costs <- list(
  standard_tmz = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 350,
    progressive = 2720,
    death = 0
  ),
  long_term_tmz = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 2453,
    progressive = 2720,
    death = 0
  )
)

# Store cycle length and time horizon
model_parameters <- list(
  cycle_length = 1,  # in months
  time_horizon = 60  # in months
)

# Define the overall survival (OS) and progression-free survival (PFS) data
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

# Define the number of cycles
time_horizon <- length(OS_standard)

# Calculate transition probabilities using the formula: 1 - exp(ln(y)/t)
transition_probs_standard <- data.frame(
  cycle = 1:time_horizon,
  prob_stable_to_death = 1 - exp(log(OS_standard) / (1:time_horizon)),
  prob_stable_to_progressive = 1 - exp(log(PFS_standard) / (1:time_horizon)),
  prob_progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / (1:time_horizon))
)

# Calculate stable to stable transition probability
transition_probs_standard$prob_stable_to_stable <- 1 - (
  transition_probs_standard$prob_stable_to_progressive + 
    transition_probs_standard$prob_stable_to_death
)

# Calculate progressive to progressive transition probability
transition_probs_standard$prob_progressive_to_progressive <- 1 - (
  transition_probs_standard$prob_progressive_to_death
)

# Store the updated transition probabilities
transition_probs_standard

# Define the overall survival (OS) and progression-free survival (PFS) data for long-term TMZ therapy
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

# Define the number of cycles
time_horizon <- length(OS_long_term)

# Calculate transition probabilities using the formula: 1 - exp(ln(y)/t)
transition_probs_long_term <- data.frame(
  cycle = 1:time_horizon,
  prob_stable_to_death = 1 - exp(log(OS_long_term) / (1:time_horizon)),
  prob_stable_to_progressive = 1 - exp(log(PFS_long_term) / (1:time_horizon)),
  prob_progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / (1:time_horizon))
)

# Calculate stable to stable transition probability
transition_probs_long_term$prob_stable_to_stable <- 1 - (
  transition_probs_long_term$prob_stable_to_progressive + 
    transition_probs_long_term$prob_stable_to_death
)

# Calculate progressive to progressive transition probability
transition_probs_long_term$prob_progressive_to_progressive <- 1 - (
  transition_probs_long_term$prob_progressive_to_death
)

# Store the updated transition probabilities
transition_probs_long_term

# Define the number of cycles
time_horizon <- 60

# Initialize Markov traces for both treatments
markov_trace_standard <- data.frame(
  cycle = 1:time_horizon,
  stable = numeric(time_horizon),
  progressive = numeric(time_horizon),
  death = numeric(time_horizon)
)

markov_trace_long_term <- data.frame(
  cycle = 1:time_horizon,
  stable = numeric(time_horizon),
  progressive = numeric(time_horizon),
  death = numeric(time_horizon)
)

# Initial state distribution
markov_trace_standard$stable[1] <- 1
markov_trace_standard$progressive[1] <- 0
markov_trace_standard$death[1] <- 0

markov_trace_long_term$stable[1] <- 1
markov_trace_long_term$progressive[1] <- 0
markov_trace_long_term$death[1] <- 0

# Compute Markov trace using the given transition logic
for (t in 2:time_horizon) {
  prev_cycle <- max(1, t - 2) # Use cycle t-2, but not before cycle 1
  
  # Standard TMZ therapy
  markov_trace_standard$stable[t] <- markov_trace_standard$stable[t-1] * transition_probs_standard$prob_stable_to_stable[prev_cycle] 
  markov_trace_standard$progressive[t] <- (markov_trace_standard$stable[t-1] * transition_probs_standard$prob_stable_to_progressive[prev_cycle]) +
    (markov_trace_standard$progressive[t-1] * transition_probs_standard$prob_progressive_to_progressive[prev_cycle])
  markov_trace_standard$death[t] <- 1 - (markov_trace_standard$stable[t] + markov_trace_standard$progressive[t])
  
  # Long-term TMZ therapy
  markov_trace_long_term$stable[t] <- markov_trace_long_term$stable[t-1] * transition_probs_long_term$prob_stable_to_stable[prev_cycle] 
  markov_trace_long_term$progressive[t] <- (markov_trace_long_term$stable[t-1] * transition_probs_long_term$prob_stable_to_progressive[prev_cycle]) +
    (markov_trace_long_term$progressive[t-1] * transition_probs_long_term$prob_progressive_to_progressive[prev_cycle])
  markov_trace_long_term$death[t] <- 1 - (markov_trace_long_term$stable[t] + markov_trace_long_term$progressive[t])
}

# Store the Markov traces
markov_trace_standard
markov_trace_long_term

# Define health utility values
utility_stable <- 0.743
utility_progressive <- 0.731
utility_decrement <- 0.02

time_horizon <- 60

discounted_utilities_progressive <- rep(utility_progressive, time_horizon)

decay_cycles <- 23 # The number of cycles over which decrement applies
start_decay <- 3 # Cycle at which decrement begins

# Apply decrement to progressive utility values from cycle 3 to 25
for (t in start_decay:(start_decay + decay_cycles - 1)) {
  discounted_utilities_progressive[t] <- max(0, utility_progressive - (t - start_decay + 1) * utility_decrement)
}

# Fix the utility value from cycle 26 onwards
final_fixed_utility <- discounted_utilities_progressive[start_decay + decay_cycles - 1]
discounted_utilities_progressive[(start_decay + decay_cycles):time_horizon] <- final_fixed_utility

# Initialize QALMs calculation
qalms_standard <- data.frame(
  cycle = 1:time_horizon,
  qalms = numeric(time_horizon)
)

qalms_long_term <- data.frame(
  cycle = 1:time_horizon,
  qalms = numeric(time_horizon)
)

# Compute QALMs for each cycle
for (t in 1:time_horizon) {
  qalms_standard$qalms[t] <- (markov_trace_standard$stable[t] * utility_stable) +
    (markov_trace_standard$progressive[t] * discounted_utilities_progressive[t])
  
  qalms_long_term$qalms[t] <- (markov_trace_long_term$stable[t] * utility_stable) +
    (markov_trace_long_term$progressive[t] * discounted_utilities_progressive[t])
}

# Calculate total QALMs
total_qalms_standard <- sum(qalms_standard$qalms)
total_qalms_long_term <- sum(qalms_long_term$qalms)

# Convert QALMs to QALYs (assuming cycle length is 1 month)
total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Store results
qalms_standard
total_qalms_standard
total_qalys_standard
qalms_long_term
total_qalms_long_term
total_qalys_long_term

# Define cost parameters
cost_standard <- c(rep(1944, 4), rep(2453, 3), rep(350, 53))
cost_long_term <- c(rep(1944, 4), rep(2453, 3), rep(2453, 53))
cost_progressive <- 2720
cost_death <- 0

time_horizon <- 60

# Initialize cost calculation
total_costs_standard <- data.frame(
  cycle = 1:time_horizon,
  costs = numeric(time_horizon)
)

total_costs_long_term <- data.frame(
  cycle = 1:time_horizon,
  costs = numeric(time_horizon)
)

# Compute costs for each cycle
for (t in 1:time_horizon) {
  total_costs_standard$costs[t] <- (markov_trace_standard$stable[t] * cost_standard[t]) +
    (markov_trace_standard$progressive[t] * cost_progressive) +
    (markov_trace_standard$death[t] * cost_death)
  
  total_costs_long_term$costs[t] <- (markov_trace_long_term$stable[t] * cost_long_term[t]) +
    (markov_trace_long_term$progressive[t] * cost_progressive) +
    (markov_trace_long_term$death[t] * cost_death)
}

# Calculate total costs
total_cost_standard <- sum(total_costs_standard$costs)
total_cost_long_term <- sum(total_costs_long_term$costs)

# Store results
total_costs_standard
total_cost_standard
total_costs_long_term
total_cost_long_term

# Calculate incremental values
incremental_costs <- total_cost_long_term - total_cost_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- total_qalys_long_term - total_qalys_standard

# Calculate ICER for QALMs and QALYs
icer_qalms <- round(incremental_costs / incremental_qalms, 0)
nicer_qalys <- round(incremental_costs / incremental_qalys, 0)

# Store results
icer_qalms
nicer_qalys
