# Load necessary libraries
library(dplyr)

# Define health utility values
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Define costs for Standard TMZ therapy
costs_standard_TMF <- list(
  stable_1_3 = 1944,
  stable_4_6 = 2453,
  stable_subsequent = 350,
  progressive = 2720,
  death = 0
)

# Define costs for Long-term TMZ therapy
costs_longterm_TMF <- list(
  stable_1_3 = 1944,
  stable_4_6 = 2453,
  stable_subsequent = 2453,
  progressive = 2720,
  death = 0
)

# Define cycle length and time horizon
model_params <- list(
  cycle_length = 1,  # in months
  time_horizon = 60  # in months
)

# Load necessary libraries
library(dplyr)

# Define the given OS and PFS data for Standard TMZ therapy
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

# Calculate transition probabilities using the given formula: 1 - exp(ln(Y)/t)
transition_probs_standard <- data.frame(
  cycle = 1:num_cycles,
  stable_to_death = 1 - exp(log(OS_standard) / (1:num_cycles)),
  stable_to_progressive = 1 - exp(log(PFS_standard) / (1:num_cycles)),
  progressive_to_death = 1 - exp(log(OS_standard - PFS_standard) / (1:num_cycles))
)

# Calculate transition probabilities for stable to stable and progressive to progressive
transition_probs_standard$stable_to_stable <- 1 - transition_probs_standard$stable_to_death - transition_probs_standard$stable_to_progressive
transition_probs_standard$progressive_to_progressive <- 1 - transition_probs_standard$progressive_to_death

# Store updated transition probabilities without printing
transition_probs_standard <- as.list(transition_probs_standard)

# Load necessary libraries
library(dplyr)

# Define the given OS and PFS data for Long-term TMZ therapy
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

# Calculate transition probabilities using the given formula: 1 - exp(ln(Y)/t)
transition_probs_long_term <- data.frame(
  cycle = 1:num_cycles,
  stable_to_death = 1 - exp(log(OS_long_term) / (1:num_cycles)),
  stable_to_progressive = 1 - exp(log(PFS_long_term) / (1:num_cycles)),
  progressive_to_death = 1 - exp(log(OS_long_term - PFS_long_term) / (1:num_cycles))
)

# Calculate transition probabilities for stable to stable and progressive to progressive
transition_probs_long_term$stable_to_stable <- 1 - transition_probs_long_term$stable_to_death - transition_probs_long_term$stable_to_progressive
transition_probs_long_term$progressive_to_progressive <- 1 - transition_probs_long_term$progressive_to_death

# Store updated transition probabilities without printing
transition_probs_long_term <- as.list(transition_probs_long_term)

# Load necessary libraries
library(dplyr)

# Define the number of cycles
num_cycles <- 60

# Initialize the Markov trace for Standard TMZ therapy
markov_trace_standard <- data.frame(
  cycle = 1:num_cycles,
  stable = rep(0, num_cycles),
  progressive = rep(0, num_cycles),
  death = rep(0, num_cycles)
)

# Set initial state distribution for Standard TMZ therapy
markov_trace_standard$stable[1] <- 1  # 100% start in the stable state
markov_trace_standard$progressive[1] <- 0
markov_trace_standard$death[1] <- 0

# Initialize the Markov trace for Long-term TMZ therapy
markov_trace_long_term <- markov_trace_standard  # Same structure

# Compute the state proportions for each cycle for both treatments
for (t in 2:num_cycles) {
  
  # Determine which transition probabilities to use
  tp_index <- ifelse(t <= 3, 1, t - 2)
  
  # Standard TMZ therapy
  markov_trace_standard$stable[t] <- markov_trace_standard$stable[t-1] * transition_probs_standard$stable_to_stable[tp_index]
  markov_trace_standard$progressive[t] <- (
    markov_trace_standard$progressive[t-1] * transition_probs_standard$progressive_to_progressive[tp_index] +
      markov_trace_standard$stable[t-1] * transition_probs_standard$stable_to_progressive[tp_index]
  )
  markov_trace_standard$death[t] <- (
    markov_trace_standard$death[t-1] +
      markov_trace_standard$stable[t-1] * transition_probs_standard$stable_to_death[tp_index] +
      markov_trace_standard$progressive[t-1] * transition_probs_standard$progressive_to_death[tp_index]
  )
  
  # Long-term TMZ therapy
  markov_trace_long_term$stable[t] <- markov_trace_long_term$stable[t-1] * transition_probs_long_term$stable_to_stable[tp_index]
  markov_trace_long_term$progressive[t] <- (
    markov_trace_long_term$progressive[t-1] * transition_probs_long_term$progressive_to_progressive[tp_index] +
      markov_trace_long_term$stable[t-1] * transition_probs_long_term$stable_to_progressive[tp_index]
  )
  markov_trace_long_term$death[t] <- (
    markov_trace_long_term$death[t-1] +
      markov_trace_long_term$stable[t-1] * transition_probs_long_term$stable_to_death[tp_index] +
      markov_trace_long_term$progressive[t-1] * transition_probs_long_term$progressive_to_death[tp_index]
  )
}

# Store results without printing
markov_trace_standard <- as.list(markov_trace_standard)
markov_trace_long_term <- as.list(markov_trace_long_term)

# Load necessary libraries
library(dplyr)

# Define utility values
utility_stable <- 0.743
utility_progressive_initial <- 0.731
utility_decrement <- 0.02
decrement_start_cycle <- 3
decrement_end_cycle <- 25
num_cycles <- 60

# Generate utility values for the progressive state, applying decrement rule
progressive_utilities <- rep(utility_progressive_initial, num_cycles)
for (t in decrement_start_cycle:decrement_end_cycle) {
  progressive_utilities[t] <- utility_progressive_initial - ((t - decrement_start_cycle + 1) * utility_decrement)
}
# Keep the utility at cycle 25 constant for remaining cycles
progressive_utilities[(decrement_end_cycle + 1):num_cycles] <- progressive_utilities[decrement_end_cycle]

# Initialize data frames to store QALMs per cycle
qalms_standard <- data.frame(
  cycle = 1:num_cycles,
  qalms = rep(0, num_cycles)
)

qalms_long_term <- qalms_standard  # Same structure for long-term therapy

# Calculate QALMs for each cycle
for (t in 1:num_cycles) {
  
  # Standard TMZ therapy
  qalms_standard$qalms[t] <- (
    markov_trace_standard$stable[t] * utility_stable +
      markov_trace_standard$progressive[t] * progressive_utilities[t]
  )
  
  # Long-term TMZ therapy
  qalms_long_term$qalms[t] <- (
    markov_trace_long_term$stable[t] * utility_stable +
      markov_trace_long_term$progressive[t] * progressive_utilities[t]
  )
}

# Calculate total QALMs for each treatment
total_qalms_standard <- sum(qalms_standard$qalms)
total_qalms_long_term <- sum(qalms_long_term$qalms)

# Store results without printing
qalms_results <- list(
  cycle_qalms_standard = qalms_standard,
  cycle_qalms_long_term = qalms_long_term,
  total_qalms_standard = total_qalms_standard,
  total_qalms_long_term = total_qalms_long_term
)

# Calculate total QALYs for each treatment
total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Store results without printing
qalys_results <- list(
  total_qalys_standard = total_qalys_standard,
  total_qalys_long_term = total_qalys_long_term
)

# Load necessary libraries
library(dplyr)

# Define the number of cycles
num_cycles <- 60

# Define cost parameters for Standard TMZ therapy
costs_standard <- c(
  stable_1_3 = 1944,   # Applies to cycles 1-4
  stable_4_6 = 2453,   # Applies to cycles 5-7
  stable_subsequent = 350, # Applies to cycles 8-60
  progressive = 2720,
  death = 0
)

# Define cost parameters for Long-term TMZ therapy
costs_long_term <- c(
  stable_1_3 = 1944,   # Applies to cycles 1-4
  stable_4_6 = 2453,   # Applies to cycles 5-7
  stable_subsequent = 2453, # Applies to cycles 8-60
  progressive = 2720,
  death = 0
)

# Assign stable state costs based on the cycle structure
assign_stable_cost <- function(cycle, costs) {
  if (cycle <= 4) return(costs["stable_1_3"])
  if (cycle <= 7) return(costs["stable_4_6"])
  return(costs["stable_subsequent"])
}

# Initialize data frames to store costs per cycle
costs_standard_trace <- data.frame(
  cycle = 1:num_cycles,
  cost = rep(0, num_cycles)
)

costs_long_term_trace <- costs_standard_trace  # Same structure for long-term therapy

# Calculate costs for each cycle
for (t in 1:num_cycles) {
  
  # Standard TMZ therapy
  stable_cost_standard <- assign_stable_cost(t, costs_standard)
  costs_standard_trace$cost[t] <- (
    markov_trace_standard$stable[t] * stable_cost_standard +
      markov_trace_standard$progressive[t] * costs_standard["progressive"] +
      markov_trace_standard$death[t] * costs_standard["death"]
  )
  
  # Long-term TMZ therapy
  stable_cost_long_term <- assign_stable_cost(t, costs_long_term)
  costs_long_term_trace$cost[t] <- (
    markov_trace_long_term$stable[t] * stable_cost_long_term +
      markov_trace_long_term$progressive[t] * costs_long_term["progressive"] +
      markov_trace_long_term$death[t] * costs_long_term["death"]
  )
}

# Calculate total costs for each treatment
total_cost_standard <- sum(costs_standard_trace$cost)
total_cost_long_term <- sum(costs_long_term_trace$cost)

# Store results without printing
cost_results <- list(
  cycle_costs_standard = costs_standard_trace,
  cycle_costs_long_term = costs_long_term_trace,
  total_cost_standard = total_cost_standard,
  total_cost_long_term = total_cost_long_term
)

# Calculate incremental costs and effects
incremental_cost <- total_cost_long_term - total_cost_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- total_qalys_long_term - total_qalys_standard

# Calculate ICERs
icer_qalms <- round(incremental_cost / incremental_qalms, 0)  # ICER per QALM
icer_qalys <- round(incremental_cost / incremental_qalys, 0)  # ICER per QALY

# Store results without printing
icer_results <- list(
  incremental_cost = incremental_cost,
  incremental_qalms = incremental_qalms,
  incremental_qalys = incremental_qalys,
  icer_qalms = icer_qalms,
  icer_qalys = icer_qalys
)
