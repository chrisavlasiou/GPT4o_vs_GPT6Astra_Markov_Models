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
model_params <- list(
  cycle_length = 1,   # 1 month per cycle
  time_horizon = 60   # 60 months
)

# Define survival probabilities
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
num_cycles <- length(OS_standard)

# Initialize transition probability lists
stable_to_death <- numeric(num_cycles)
stable_to_progressive <- numeric(num_cycles)
progressive_to_death <- numeric(num_cycles)
stable_to_stable <- numeric(num_cycles)
progressive_to_progressive <- numeric(num_cycles)

# Calculate transition probabilities using formula 1 - exp(ln(y)/t)
for (t in 1:num_cycles) {
  stable_to_death[t] <- 1 - exp(log(OS_standard[t]) / t)
  stable_to_progressive[t] <- 1 - exp(log(PFS_standard[t]) / t)
  progressive_to_death[t] <- 1 - exp(log(OS_standard[t] - PFS_standard[t]) / t)
  stable_to_stable[t] <- 1 - stable_to_progressive[t] - stable_to_death[t]
  progressive_to_progressive[t] <- 1 - progressive_to_death[t]
}

# Store transition probabilities in a list
transition_probs_standard <- list(
  stable_to_death = stable_to_death,
  stable_to_progressive = stable_to_progressive,
  progressive_to_death = progressive_to_death,
  stable_to_stable = stable_to_stable,
  progressive_to_progressive = progressive_to_progressive
)

# The transition probabilities are stored but not printed

# Define survival probabilities for Long-term TMZ therapy
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
num_cycles <- length(OS_long_term)

# Initialize transition probability lists
stable_to_death <- numeric(num_cycles)
stable_to_progressive <- numeric(num_cycles)
progressive_to_death <- numeric(num_cycles)
stable_to_stable <- numeric(num_cycles)
progressive_to_progressive <- numeric(num_cycles)

# Calculate transition probabilities using formula 1 - exp(ln(y)/t)
for (t in 1:num_cycles) {
  stable_to_death[t] <- 1 - exp(log(OS_long_term[t]) / t)
  stable_to_progressive[t] <- 1 - exp(log(PFS_long_term[t]) / t)
  progressive_to_death[t] <- 1 - exp(log(OS_long_term[t] - PFS_long_term[t]) / t)
  stable_to_stable[t] <- 1 - stable_to_progressive[t] - stable_to_death[t]
  progressive_to_progressive[t] <- 1 - progressive_to_death[t]
}

# Store transition probabilities in a list
transition_probs_long_term <- list(
  stable_to_death = stable_to_death,
  stable_to_progressive = stable_to_progressive,
  progressive_to_death = progressive_to_death,
  stable_to_stable = stable_to_stable,
  progressive_to_progressive = progressive_to_progressive
)

# The transition probabilities are stored but not printed

# Define number of cycles
num_cycles <- 60

# Initialize Markov trace matrices for both treatments
trace_standard <- matrix(0, nrow = num_cycles, ncol = 3, 
                         dimnames = list(1:num_cycles, c("Stable", "Progressive", "Death")))
trace_long_term <- matrix(0, nrow = num_cycles, ncol = 3, 
                          dimnames = list(1:num_cycles, c("Stable", "Progressive", "Death")))

# Initial state distribution (all patients start in Stable state)
trace_standard[1, ] <- c(1, 0, 0)
trace_long_term[1, ] <- c(1, 0, 0)

# Compute Markov trace for both treatments
for (t in 2:num_cycles) {
  # Use transition probabilities from two cycles earlier, except for cycles 2 and 3
  prev_cycle <- ifelse(t <= 3, 1, t - 2)
  
  # Standard TMZ therapy transitions
  trace_standard[t, "Stable"] <- trace_standard[t - 1, "Stable"] * transition_probs_standard$stable_to_stable[prev_cycle]
  trace_standard[t, "Progressive"] <- trace_standard[t - 1, "Stable"] * transition_probs_standard$stable_to_progressive[prev_cycle] +
    trace_standard[t - 1, "Progressive"] * transition_probs_standard$progressive_to_progressive[prev_cycle]
  trace_standard[t, "Death"] <- trace_standard[t - 1, "Stable"] * transition_probs_standard$stable_to_death[prev_cycle] +
    trace_standard[t - 1, "Progressive"] * transition_probs_standard$progressive_to_death[prev_cycle] +
    trace_standard[t - 1, "Death"]
  
  # Long-term TMZ therapy transitions
  trace_long_term[t, "Stable"] <- trace_long_term[t - 1, "Stable"] * transition_probs_long_term$stable_to_stable[prev_cycle]
  trace_long_term[t, "Progressive"] <- trace_long_term[t - 1, "Stable"] * transition_probs_long_term$stable_to_progressive[prev_cycle] +
    trace_long_term[t - 1, "Progressive"] * transition_probs_long_term$progressive_to_progressive[prev_cycle]
  trace_long_term[t, "Death"] <- trace_long_term[t - 1, "Stable"] * transition_probs_long_term$stable_to_death[prev_cycle] +
    trace_long_term[t - 1, "Progressive"] * transition_probs_long_term$progressive_to_death[prev_cycle] +
    trace_long_term[t - 1, "Death"]
}

# Store the Markov traces
markov_trace <- list(
  standard = trace_standard,
  long_term = trace_long_term
)

# The Markov trace matrices are stored but not printed

# Define utility values
utility_stable <- 0.743
utility_progressive <- 0.731
decrement_per_month <- 0.02

# Initialize QALMs storage
qalms_standard <- numeric(num_cycles)
qalms_long_term <- numeric(num_cycles)

# Create a vector for progressive state utilities accounting for decrements
progressive_utilities <- rep(utility_progressive, num_cycles)
for (t in 3:25) {
  progressive_utilities[t] <- max(utility_progressive - (t - 2) * decrement_per_month, 0)
}
progressive_utilities[26:num_cycles] <- progressive_utilities[25] # Fix utility after cycle 25

# Calculate QALMs for each cycle
for (t in 1:num_cycles) {
  qalms_standard[t] <- markov_trace$standard[t, "Stable"] * utility_stable +
    markov_trace$standard[t, "Progressive"] * progressive_utilities[t]
  
  qalms_long_term[t] <- markov_trace$long_term[t, "Stable"] * utility_stable +
    markov_trace$long_term[t, "Progressive"] * progressive_utilities[t]
}

# Calculate total QALMs by summing over all cycles
total_qalms_standard <- sum(qalms_standard)
total_qalms_long_term <- sum(qalms_long_term)

# Convert total QALMs to total QALYs (divide by 12 months per year)
total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Store results
qalms_results <- list(
  cycle_qalms_standard = qalms_standard,
  cycle_qalms_long_term = qalms_long_term,
  total_qalms_standard = total_qalms_standard,
  total_qalms_long_term = total_qalms_long_term,
  total_qalys_standard = total_qalys_standard,
  total_qalys_long_term = total_qalys_long_term
)

# The QALMs and QALYs results are stored but not printed

# Define cost values
costs_standard <- c(rep(1944, 4), rep(2453, 3), rep(350, 53))
costs_long_term <- c(rep(1944, 4), rep(2453, 3), rep(2453, 53))
cost_progressive <- 2720
cost_death <- 0

# Initialize cost storage
costs_per_cycle_standard <- numeric(num_cycles)
costs_per_cycle_long_term <- numeric(num_cycles)

# Calculate costs for each cycle
for (t in 1:num_cycles) {
  costs_per_cycle_standard[t] <- markov_trace$standard[t, "Stable"] * costs_standard[t] +
    markov_trace$standard[t, "Progressive"] * cost_progressive +
    markov_trace$standard[t, "Death"] * cost_death
  
  costs_per_cycle_long_term[t] <- markov_trace$long_term[t, "Stable"] * costs_long_term[t] +
    markov_trace$long_term[t, "Progressive"] * cost_progressive +
    markov_trace$long_term[t, "Death"] * cost_death
}

# Calculate total costs by summing over all cycles
total_cost_standard <- sum(costs_per_cycle_standard)
total_cost_long_term <- sum(costs_per_cycle_long_term)

# Store results
cost_results <- list(
  cycle_costs_standard = costs_per_cycle_standard,
  cycle_costs_long_term = costs_per_cycle_long_term,
  total_cost_standard = total_cost_standard,
  total_cost_long_term = total_cost_long_term
)

# The cost results are stored but not printed

# Calculate incremental differences
incremental_cost <- cost_results$total_cost_long_term - cost_results$total_cost_standard
incremental_qalms <- qalms_results$total_qalms_long_term - qalms_results$total_qalms_standard
incremental_qalys <- qalms_results$total_qalys_long_term - qalms_results$total_qalys_standard

# Calculate ICERs
icer_qalms <- round(incremental_cost / incremental_qalms, 0)
nicer_qalys <- round(incremental_cost / incremental_qalys, 0)

# Store results
icer_results <- list(
  incremental_cost = incremental_cost,
  incremental_qalms = incremental_qalms,
  incremental_qalys = incremental_qalys,
  icer_qalms = nicer_qalms,
  icer_qalys = nicer_qalys
)

# The ICER results are stored but not printed
