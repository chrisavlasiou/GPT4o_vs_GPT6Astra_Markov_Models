# Store input data for the Markov model without printing

# Health utility data
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Costs data
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

# Model parameters
model_params <- list(
  cycle_length = 1,    # in months
  time_horizon = 60    # in months
)

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

# Define function to calculate transition probabilities
calculate_transition_probability <- function(y, t) {
  return(1 - exp(log(y) / t))
}

# Initialize transition probability lists
transition_probs_standard <- list(
  stable_to_death = numeric(length(OS_standard)),
  stable_to_progressive = numeric(length(PFS_standard)),
  progressive_to_death = numeric(length(OS_standard))
)

# Calculate transition probabilities for each cycle
for (t in 1:length(OS_standard)) {
  transition_probs_standard$stable_to_death[t] <- calculate_transition_probability(OS_standard[t], t)
  transition_probs_standard$stable_to_progressive[t] <- calculate_transition_probability(PFS_standard[t], t)
  transition_probs_standard$progressive_to_death[t] <- calculate_transition_probability(OS_standard[t] - PFS_standard[t], t)
}

# Store transition probabilities without printing
invisible(transition_probs_standard)

# Calculate transition probabilities for stable to stable
transition_probs_standard$stable_to_stable <- 1 - (
  transition_probs_standard$stable_to_death + 
    transition_probs_standard$stable_to_progressive
)

# Calculate transition probabilities for progressive to progressive
transition_probs_standard$progressive_to_progressive <- 1 - (
  transition_probs_standard$progressive_to_death
)

# Ensure all transition probabilities are stored without printing
invisible(transition_probs_standard)

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

# Define function to calculate transition probabilities
calculate_transition_probability <- function(y, t) {
  return(1 - exp(log(y) / t))
}

# Initialize transition probability lists
transition_probs_long_term <- list(
  stable_to_death = numeric(length(OS_long_term)),
  stable_to_progressive = numeric(length(PFS_long_term)),
  progressive_to_death = numeric(length(OS_long_term))
)

# Calculate transition probabilities for each cycle
for (t in 1:length(OS_long_term)) {
  transition_probs_long_term$stable_to_death[t] <- calculate_transition_probability(OS_long_term[t], t)
  transition_probs_long_term$stable_to_progressive[t] <- calculate_transition_probability(PFS_long_term[t], t)
  transition_probs_long_term$progressive_to_death[t] <- calculate_transition_probability(OS_long_term[t] - PFS_long_term[t], t)
}

# Store transition probabilities without printing
invisible(transition_probs_long_term)

# Calculate transition probabilities for stable to stable
transition_probs_long_term$stable_to_stable <- 1 - (
  transition_probs_long_term$stable_to_death + 
    transition_probs_long_term$stable_to_progressive
)

# Calculate transition probabilities for progressive to progressive
transition_probs_long_term$progressive_to_progressive <- 1 - (
  transition_probs_long_term$progressive_to_death
)

# Ensure all transition probabilities are stored without printing
invisible(transition_probs_long_term)

# Define model parameters
n_cycles <- 60  # Total cycles
initial_state <- c(stable = 1, progressive = 0, death = 0)  # All patients start in Stable state

# Initialize Markov trace matrices for both treatments
markov_trace_standard <- matrix(0, nrow = n_cycles, ncol = 3, 
                                dimnames = list(1:n_cycles, c("Stable", "Progressive", "Death")))
markov_trace_long_term <- matrix(0, nrow = n_cycles, ncol = 3, 
                                 dimnames = list(1:n_cycles, c("Stable", "Progressive", "Death")))

# Set initial state distributions (Cycle 1)
markov_trace_standard[1, ] <- initial_state
markov_trace_long_term[1, ] <- initial_state

# Function to apply transitions for a given treatment's Markov trace
calculate_markov_trace <- function(transition_probs, trace_matrix) {
  for (t in 2:n_cycles) {
    # Select transition probabilities: Use probabilities from cycle (t-2), except for cycles 2 and 3 which use cycle 1
    if (t == 2 || t == 3) {
      cycle_index <- 1
    } else {
      cycle_index <- t - 2
    }
    
    # Extract probabilities
    p_stable_to_stable <- transition_probs$stable_to_stable[cycle_index]
    p_stable_to_progressive <- transition_probs$stable_to_progressive[cycle_index]
    p_stable_to_death <- transition_probs$stable_to_death[cycle_index]
    
    p_progressive_to_progressive <- transition_probs$progressive_to_progressive[cycle_index]
    p_progressive_to_death <- transition_probs$progressive_to_death[cycle_index]
    
    # Apply transition probabilities
    trace_matrix[t, "Stable"] <- trace_matrix[t - 1, "Stable"] * p_stable_to_stable
    trace_matrix[t, "Progressive"] <- (trace_matrix[t - 1, "Stable"] * p_stable_to_progressive) + 
      (trace_matrix[t - 1, "Progressive"] * p_progressive_to_progressive)
    trace_matrix[t, "Death"] <- trace_matrix[t - 1, "Death"] + 
      (trace_matrix[t - 1, "Stable"] * p_stable_to_death) + 
      (trace_matrix[t - 1, "Progressive"] * p_progressive_to_death)
  }
  return(trace_matrix)
}

# Calculate Markov trace for both Standard and Long-term TMZ therapy
markov_trace_standard <- calculate_markov_trace(transition_probs_standard, markov_trace_standard)
markov_trace_long_term <- calculate_markov_trace(transition_probs_long_term, markov_trace_long_term)

# Store results without printing
invisible(list(markov_trace_standard = markov_trace_standard, markov_trace_long_term = markov_trace_long_term))

# Define health utilities
utility_stable <- 0.743
utility_progressive_initial <- 0.731
utility_decrement <- 0.02
decrement_start_cycle <- 3  # Starts at cycle 3
decrement_end_cycle <- 25   # Ends at cycle 25

# Initialize matrices to store QALMs for each cycle
QALMs_standard <- numeric(n_cycles)
QALMs_long_term <- numeric(n_cycles)

# Function to calculate QALMs for each cycle
calculate_QALMs <- function(markov_trace) {
  QALMs <- numeric(n_cycles)
  
  for (t in 1:n_cycles) {
    # Determine utility for Progressive state based on cycle number
    if (t < decrement_start_cycle) {
      utility_progressive <- utility_progressive_initial
    } else if (t <= decrement_end_cycle) {
      utility_progressive <- max(utility_progressive_initial - (t - decrement_start_cycle + 1) * utility_decrement, 0)  # Ensure utility does not go below 0
    } else {
      utility_progressive <- max(utility_progressive_initial - (decrement_end_cycle - decrement_start_cycle + 1) * utility_decrement, 0)
    }
    
    # Compute QALMs for the cycle
    QALMs[t] <- (markov_trace[t, "Stable"] * utility_stable) + 
      (markov_trace[t, "Progressive"] * utility_progressive)
  }
  
  return(QALMs)
}

# Calculate QALMs for both Standard and Long-term TMZ therapy
QALMs_standard <- calculate_QALMs(markov_trace_standard)
QALMs_long_term <- calculate_QALMs(markov_trace_long_term)

# Compute total QALMs by summing over all cycles
total_QALMs_standard <- sum(QALMs_standard)
total_QALMs_long_term <- sum(QALMs_long_term)

# Store results without printing
invisible(list(
  QALMs_standard = QALMs_standard, 
  QALMs_long_term = QALMs_long_term, 
  total_QALMs_standard = total_QALMs_standard, 
  total_QALMs_long_term = total_QALMs_long_term
))

# Convert total QALMs to total QALYs
total_QALYs_standard <- total_QALMs_standard / 12
total_QALYs_long_term <- total_QALMs_long_term / 12

# Store results without printing
invisible(list(
  total_QALYs_standard = total_QALYs_standard, 
  total_QALYs_long_term = total_QALYs_long_term
))

# Define cost structure for Standard TMZ therapy
cost_standard <- numeric(n_cycles)
cost_long_term <- numeric(n_cycles)

# Assign costs based on cycle range
for (t in 1:n_cycles) {
  if (t <= 4) {  # Cycles 1-4 (Months 1-3)
    cost_standard[t] <- costs$standard_TMX$stable_1_3
    cost_long_term[t] <- costs$long_term_TMX$stable_1_3
  } else if (t <= 7) {  # Cycles 5-7 (Months 4-6)
    cost_standard[t] <- costs$standard_TMX$stable_4_6
    cost_long_term[t] <- costs$long_term_TMX$stable_4_6
  } else {  # Cycles 8-60 (Subsequent months)
    cost_standard[t] <- costs$standard_TMX$stable_subsequent
    cost_long_term[t] <- costs$long_term_TMX$stable_subsequent
  }
}

# Initialize cost vectors per cycle
costs_per_cycle_standard <- numeric(n_cycles)
costs_per_cycle_long_term <- numeric(n_cycles)

# Function to calculate costs for each cycle
calculate_costs <- function(markov_trace, cost_structure) {
  costs_per_cycle <- numeric(n_cycles)
  
  for (t in 1:n_cycles) {
    # Calculate total cost per cycle
    costs_per_cycle[t] <- (markov_trace[t, "Stable"] * cost_structure[t]) +
      (markov_trace[t, "Progressive"] * costs$standard_TMX$progressive) +
      (markov_trace[t, "Death"] * costs$standard_TMX$death)  # Death cost is 0
  }
  
  return(costs_per_cycle)
}

# Compute costs for each cycle
costs_per_cycle_standard <- calculate_costs(markov_trace_standard, cost_standard)
costs_per_cycle_long_term <- calculate_costs(markov_trace_long_term, cost_long_term)

# Calculate total costs
total_cost_standard <- sum(costs_per_cycle_standard)
total_cost_long_term <- sum(costs_per_cycle_long_term)

# Store results without printing
invisible(list(
  costs_per_cycle_standard = costs_per_cycle_standard, 
  costs_per_cycle_long_term = costs_per_cycle_long_term, 
  total_cost_standard = total_cost_standard, 
  total_cost_long_term = total_cost_long_term
))

# Calculate Incremental Cost-Effectiveness Ratio (ICER)

# Compute incremental values
incremental_cost <- total_cost_long_term - total_cost_standard
incremental_QALMs <- total_QALMs_long_term - total_QALMs_standard
incremental_QALYs <- incremental_QALMs / 12  # Convert to QALYs

# Compute ICERs and round to whole numbers
ICER_QALMs <- round(incremental_cost / incremental_QALMs, 0)
ICER_QALYs <- round(incremental_cost / incremental_QALYs, 0)

# Store results without printing
invisible(list(
  ICER_QALMs = ICER_QALMs, 
  ICER_QALYs = ICER_QALYs
))
