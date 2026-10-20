# Store health utility data
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Store costs data
costs <- list(
  standard_TMX = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 350,
    progressive = 2720,
    death = 0
  ),
  long_term_TMX = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 2453,
    progressive = 2720,
    death = 0
  )
)

# Define cycle length and time horizon
cycle_length <- 1  # in months
time_horizon <- 60 # total duration in months

# Store OS and PFS data
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

# Compute transition probabilities
transition_probs_standard <- data.frame(
  cycle = 1:60,
  prob_stable_to_death = 1 - exp(log(OS_standard) / (1:60)),
  prob_stable_to_progressive = 1 - exp(log(PFS_standard) / (1:60)),
  prob_progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / (1:60))
)

# Compute stable to stable and progressive to progressive probabilities
prob_stable_to_stable <- 1 - transition_probs_standard$prob_stable_to_progressive - transition_probs_standard$prob_stable_to_death
prob_progressive_to_progressive <- 1 - transition_probs_standard$prob_progressive_to_death

# Append to transition probabilities data frame
transition_probs_standard$prob_stable_to_stable <- prob_stable_to_stable
transition_probs_standard$prob_progressive_to_progressive <- prob_progressive_to_progressive

# Store OS and PFS data for Long-term TMZ therapy
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

# Compute transition probabilities for Long-term TMZ therapy
transition_probs_long_term <- data.frame(
  cycle = 1:60,
  prob_stable_to_death = 1 - exp(log(OS_long_term) / (1:60)),
  prob_stable_to_progressive = 1 - exp(log(PFS_long_term) / (1:60)),
  prob_progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / (1:60))
)

# Compute stable to stable and progressive to progressive probabilities
prob_stable_to_stable <- 1 - transition_probs_long_term$prob_stable_to_progressive - transition_probs_long_term$prob_stable_to_death
prob_progressive_to_progressive <- 1 - transition_probs_long_term$prob_progressive_to_death

# Append to transition probabilities data frame
transition_probs_long_term$prob_stable_to_stable <- prob_stable_to_stable
transition_probs_long_term$prob_progressive_to_progressive <- prob_progressive_to_progressive

# Define number of cycles
num_cycles <- 60

# Initialize Markov traces for Standard TMZ and Long-term TMZ
tm_standard <- matrix(0, nrow = num_cycles, ncol = 3)
tm_long_term <- matrix(0, nrow = num_cycles, ncol = 3)
colnames(tm_standard) <- colnames(tm_long_term) <- c("Stable", "Progressive", "Death")

# Initial state distribution
initial_state <- c(1, 0, 0)  # All patients start in Stable state
tm_standard[1, ] <- initial_state
tm_long_term[1, ] <- initial_state

# Compute Markov trace using transition probabilities
for (t in 2:num_cycles) {
  # Use transition probabilities from two cycles earlier (except for cycle 2 and 3, which use cycle 1 probabilities)
  tp_index <- max(1, t - 2)
  
  # Standard TMZ transitions
  tm_standard[t, "Stable"] <- tm_standard[t - 1, "Stable"] * transition_probs_standard$prob_stable_to_stable[tp_index]
  tm_standard[t, "Progressive"] <- (tm_standard[t - 1, "Stable"] * transition_probs_standard$prob_stable_to_progressive[tp_index]) +
    (tm_standard[t - 1, "Progressive"] * transition_probs_standard$prob_progressive_to_progressive[tp_index])
  tm_standard[t, "Death"] <- 1 - (tm_standard[t, "Stable"] + tm_standard[t, "Progressive"])
  
  # Long-term TMZ transitions
  tm_long_term[t, "Stable"] <- tm_long_term[t - 1, "Stable"] * transition_probs_long_term$prob_stable_to_stable[tp_index]
  tm_long_term[t, "Progressive"] <- (tm_long_term[t - 1, "Stable"] * transition_probs_long_term$prob_stable_to_progressive[tp_index]) +
    (tm_long_term[t - 1, "Progressive"] * transition_probs_long_term$prob_progressive_to_progressive[tp_index])
  tm_long_term[t, "Death"] <- 1 - (tm_long_term[t, "Stable"] + tm_long_term[t, "Progressive"])
}

# Convert matrices to data frames
markov_trace_standard <- as.data.frame(tm_standard)
markov_trace_standard$Cycle <- 1:num_cycles

markov_trace_long_term <- as.data.frame(tm_long_term)
markov_trace_long_term$Cycle <- 1:num_cycles

# Define health state utilities
utility_stable <- 0.743
utility_progressive <- 0.731
utility_decrement <- 0.02

total_cycles <- 60

discounted_utilities_progressive <- rep(utility_progressive, total_cycles)

discounted_utilities_progressive[3:25] <- utility_progressive - ((1:23) * utility_decrement)
discounted_utilities_progressive[discounted_utilities_progressive < 0] <- 0  # Ensure utility does not go below 0

discounted_utilities_progressive[26:total_cycles] <- discounted_utilities_progressive[25]  # Keep fixed after cycle 25

# Calculate QALMs for each cycle
qalm_standard <- markov_trace_standard$Stable * utility_stable + markov_trace_standard$Progressive * discounted_utilities_progressive
qalm_long_term <- markov_trace_long_term$Stable * utility_stable + markov_trace_long_term$Progressive * discounted_utilities_progressive

# Calculate total QALMs
total_qalm_standard <- sum(qalm_standard)
total_qalm_long_term <- sum(qalm_long_term)

# Convert QALMs to QALYs (divide by 12 since 1 year = 12 months)
total_qaly_standard <- total_qalm_standard / 12
total_qaly_long_term <- total_qalm_long_term / 12

# Store results in a data frame
qalm_results <- data.frame(
  Cycle = 1:total_cycles,
  QALM_Standard = qalm_standard,
  QALM_Long_Term = qalm_long_term
)

# Display total QALMs and QALYs
total_qalm_results <- data.frame(
  Treatment = c("Standard TMZ", "Long-term TMZ"),
  Total_QALMs = c(total_qalm_standard, total_qalm_long_term),
  Total_QALYs = c(total_qaly_standard, total_qaly_long_term)
)

# Define cost structure for both treatments
cost_standard <- c(rep(1944, 4), rep(2453, 3), rep(350, 53))
cost_long_term <- c(rep(1944, 4), rep(2453, 3), rep(2453, 53))
cost_progressive <- 2720
cost_death <- 0

total_cycles <- 60

# Calculate cost for each cycle
costs_standard <- markov_trace_standard$Stable * cost_standard + 
  markov_trace_standard$Progressive * cost_progressive + 
  markov_trace_standard$Death * cost_death

costs_long_term <- markov_trace_long_term$Stable * cost_long_term + 
  markov_trace_long_term$Progressive * cost_progressive + 
  markov_trace_long_term$Death * cost_death

# Calculate total costs
total_cost_standard <- sum(costs_standard)
total_cost_long_term <- sum(costs_long_term)

# Store results in a data frame
cost_results <- data.frame(
  Cycle = 1:total_cycles,
  Cost_Standard = costs_standard,
  Cost_Long_Term = costs_long_term
)

# Display total costs
total_cost_results <- data.frame(
  Treatment = c("Standard TMZ", "Long-term TMZ"),
  Total_Costs = c(total_cost_standard, total_cost_long_term)
)

# Calculate Incremental Cost-Effectiveness Ratio (ICER)
incremental_cost <- total_cost_long_term - total_cost_standard
incremental_qalm <- total_qalm_long_term - total_qalm_standard
incremental_qaly <- total_qaly_long_term - total_qaly_standard

# Compute ICERs
icer_qalm <- round(incremental_cost / incremental_qalm, 0)
nicer_qaly <- round(incremental_cost / incremental_qaly, 0)

# Store ICER results in a data frame
icer_results <- data.frame(
  Metric = c("ICER per QALM", "ICER per QALY"),
  Value = c(nicer_qalm, nicer_qaly)
)
