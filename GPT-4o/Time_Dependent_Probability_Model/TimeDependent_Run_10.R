# Store input parameters

# Health utility data
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Costs data
costs <- list(
  standard_TMz = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 350,
    progressive = 2720,
    death = 0
  ),
  long_term_TMz = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 2453,
    progressive = 2720,
    death = 0
  )
)

# Cycle length and time horizon
time_params <- list(
  cycle_length = 1,  # in months
  time_horizon = 60  # in months
)

# Define OS and PFS data
OS_standard <- c(0.988, 0.970, 0.950, 0.913, 0.888, 0.863, 0.813, 0.782, 0.733, 0.675, 0.650, 0.611, 0.550, 0.525, 0.488, 0.450, 0.430, 0.394, 0.350, 0.325, 0.313, 0.300, 0.275, 0.265, 0.240, 0.225, 0.213, 0.213, 0.213, 0.213, 0.188, 0.188, 0.188, 0.188, 0.188, 0.188, 0.188, 0.188, 0.188, 0.188, 0.094, 0.087, 0.081, 0.075, 0.070, 0.065, 0.060, 0.056, 0.052, 0.048, 0.045, 0.041, 0.038, 0.036, 0.033, 0.030, 0.028, 0.026, 0.024, 0.022)
PFS_standard <- c(0.978, 0.968, 0.800, 0.738, 0.643, 0.539, 0.493, 0.459, 0.417, 0.368, 0.314, 0.269, 0.259, 0.228, 0.227, 0.211, 0.197, 0.184, 0.174, 0.157, 0.146, 0.136, 0.126, 0.107, 0.097, 0.097, 0.097, 0.093, 0.093, 0.093, 0.071, 0.057, 0.033, 0.033, 0.033, 0.033, 0.024, 0.022, 0.020, 0.018, 0.016, 0.014, 0.013, 0.011, 0.010, 0.009, 0.008, 0.007, 0.007, 0.006, 0.005, 0.005, 0.004, 0.004, 0.003, 0.003, 0.003, 0.003, 0.002, 0.002)

# Number of cycles
time_horizon <- length(OS_standard)

# Calculate transition probabilities
transition_probs_standard <- data.frame(
  cycle = 1:time_horizon,
  stable_to_death = 1 - exp(log(OS_standard) / (1:time_horizon)),
  stable_to_progressive = 1 - exp(log(PFS_standard) / (1:time_horizon)),
  progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / (1:time_horizon))
)

# Calculate self-transition probabilities
transition_probs_standard$stable_to_stable <- 1 - transition_probs_standard$stable_to_progressive - transition_probs_standard$stable_to_death
transition_probs_standard$progressive_to_progressive <- 1 - transition_probs_standard$progressive_to_death

# Store updated transition probabilities (not printing as per instructions)

# Define OS and PFS data for long-term TMZ therapy
OS_long_term <- c(1.000, 1.000, 0.982, 0.930, 0.895, 0.860, 0.825, 0.789, 0.737, 0.702, 0.667, 0.649, 0.614, 0.579, 0.544, 0.526, 0.491, 0.456, 0.421, 0.404, 0.368, 0.351, 0.351, 0.351, 0.351, 0.333, 0.333, 0.316, 0.316, 0.296, 0.296, 0.296, 0.296, 0.296, 0.296, 0.269, 0.241, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215, 0.215)
PFS_long_term <- c(0.947, 0.912, 0.877, 0.754, 0.699, 0.607, 0.497, 0.478, 0.421, 0.402, 0.364, 0.325, 0.306, 0.306, 0.306, 0.287, 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.244, 0.244, 0.244, 0.244, 0.244, 0.244, 0.244, 0.203, 0.203, 0.203, 0.203, 0.203, 0.203, 0.203, 0.203, 0.203, 0.203, 0.203, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152, 0.152)

# Number of cycles
time_horizon <- length(OS_long_term)

# Calculate transition probabilities
transition_probs_long_term <- data.frame(
  cycle = 1:time_horizon,
  stable_to_death = 1 - exp(log(OS_long_term) / (1:time_horizon)),
  stable_to_progressive = 1 - exp(log(PFS_long_term) / (1:time_horizon)),
  progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / (1:time_horizon))
)

# Calculate self-transition probabilities
transition_probs_long_term$stable_to_stable <- 1 - transition_probs_long_term$stable_to_progressive - transition_probs_long_term$stable_to_death
transition_probs_long_term$progressive_to_progressive <- 1 - transition_probs_long_term$progressive_to_death

# Store updated transition probabilities (not printing as per instructions)

# Define initial state distribution
initial_state <- c(stable = 1, progressive = 0, death = 0)

# Number of cycles
time_horizon <- 60

# Initialize Markov traces for both treatments
markov_trace_standard <- matrix(0, nrow = time_horizon, ncol = 3, 
                                dimnames = list(NULL, c("stable", "progressive", "death")))
markov_trace_long_term <- matrix(0, nrow = time_horizon, ncol = 3, 
                                 dimnames = list(NULL, c("stable", "progressive", "death")))

# Set initial state distribution (Cycle 1)
markov_trace_standard[1, ] <- initial_state
markov_trace_long_term[1, ] <- initial_state

# Iterate through cycles to compute Markov trace
for (t in 2:time_horizon) {
  # Select transition probabilities from t-2, or from cycle 1 for cycles 2 and 3
  trans_index <- ifelse(t < 3, 1, t - 2)
  
  # Standard TMZ therapy
  markov_trace_standard[t, "stable"] <- markov_trace_standard[t - 1, "stable"] * 
    transition_probs_standard$stable_to_stable[trans_index]
  markov_trace_standard[t, "progressive"] <- markov_trace_standard[t - 1, "stable"] * 
    transition_probs_standard$stable_to_progressive[trans_index] +
    markov_trace_standard[t - 1, "progressive"] * 
    transition_probs_standard$progressive_to_progressive[trans_index]
  markov_trace_standard[t, "death"] <- 1 - (markov_trace_standard[t, "stable"] + 
                                              markov_trace_standard[t, "progressive"])
  
  # Long-term TMZ therapy
  markov_trace_long_term[t, "stable"] <- markov_trace_long_term[t - 1, "stable"] * 
    transition_probs_long_term$stable_to_stable[trans_index]
  markov_trace_long_term[t, "progressive"] <- markov_trace_long_term[t - 1, "stable"] * 
    transition_probs_long_term$stable_to_progressive[trans_index] +
    markov_trace_long_term[t - 1, "progressive"] * 
    transition_probs_long_term$progressive_to_progressive[trans_index]
  markov_trace_long_term[t, "death"] <- 1 - (markov_trace_long_term[t, "stable"] + 
                                               markov_trace_long_term[t, "progressive"])
}

# Store Markov traces (not printing as per instructions)

# Define health state utility values
utility_stable <- 0.743
utility_progressive <- 0.731
utility_decrement <- 0.02

# Initialize QALMs for both treatments
QALMs_standard <- numeric(time_horizon)
QALMs_long_term <- numeric(time_horizon)

# Apply decrement for progressive state starting from cycle 3 to cycle 25
progressive_utilities <- rep(utility_progressive, time_horizon)
for (t in 3:25) {
  progressive_utilities[t] <- progressive_utilities[t - 1] - utility_decrement
}
# Fix the last decremented value for cycles 26-60
progressive_utilities[26:time_horizon] <- progressive_utilities[25]

# Calculate QALMs for each cycle
for (t in 1:time_horizon) {
  QALMs_standard[t] <- (markov_trace_standard[t, "stable"] * utility_stable) +
    (markov_trace_standard[t, "progressive"] * progressive_utilities[t])
  
  QALMs_long_term[t] <- (markov_trace_long_term[t, "stable"] * utility_stable) +
    (markov_trace_long_term[t, "progressive"] * progressive_utilities[t])
}

# Calculate total QALMs
Total_QALMs_standard <- sum(QALMs_standard)
Total_QALMs_long_term <- sum(QALMs_long_term)

# Convert QALMs to QALYs (assuming 12 months per QALY)
Total_QALYs_standard <- Total_QALMs_standard / 12
Total_QALYs_long_term <- Total_QALMs_long_term / 12

# Store QALYs results (not printing as per instructions)

# Define cost values
cost_standard <- c(rep(1944, 4), rep(2453, 3), rep(350, 53))
cost_long_term <- c(rep(1944, 4), rep(2453, 3), rep(2453, 53))
cost_progressive <- 2720
cost_death <- 0

# Initialize cost vectors
costs_standard <- numeric(time_horizon)
costs_long_term <- numeric(time_horizon)

# Calculate costs for each cycle
for (t in 1:time_horizon) {
  costs_standard[t] <- (markov_trace_standard[t, "stable"] * cost_standard[t]) +
    (markov_trace_standard[t, "progressive"] * cost_progressive) +
    (markov_trace_standard[t, "death"] * cost_death)
  
  costs_long_term[t] <- (markov_trace_long_term[t, "stable"] * cost_long_term[t]) +
    (markov_trace_long_term[t, "progressive"] * cost_progressive) +
    (markov_trace_long_term[t, "death"] * cost_death)
}

# Calculate total costs
Total_Costs_standard <- sum(costs_standard)
Total_Costs_long_term <- sum(costs_long_term)

# Store cost results (not printing as per instructions)

# Calculate incremental values
incremental_QALMs <- Total_QALMs_long_term - Total_QALMs_standard
incremental_QALYs <- Total_QALYs_long_term - Total_QALYs_standard
incremental_Costs <- Total_Costs_long_term - Total_Costs_standard

# Calculate ICERs (without decimals)
ICER_QALMs <- round(incremental_Costs / incremental_QALMs, 0)
ICER_QALYs <- round(incremental_Costs / incremental_QALYs, 0)

# Store ICER results (not printing as per instructions)
