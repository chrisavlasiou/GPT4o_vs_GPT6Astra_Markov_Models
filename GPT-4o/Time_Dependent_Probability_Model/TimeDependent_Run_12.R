# Define health utility values
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Define cost data for Standard TMZ therapy
costs_standard_tmz <- list(
  stable = c(1944, 1944, 1944, 2453, 2453, 2453), # First 6 months
  stable_subsequent = 350, # Cost from month 7 onwards
  progressive = 2720,
  death = 0
)

# Define cost data for Long-term TMZ therapy
costs_longterm_tmz <- list(
  stable = c(1944, 1944, 1944, 2453, 2453, 2453), # First 6 months
  stable_subsequent = 2453, # Cost from month 7 onwards
  progressive = 2720,
  death = 0
)

# Define cycle length and time horizon
model_parameters <- list(
  cycle_length = 1, # 1 month per cycle
  time_horizon = 60 # 60 months in total
)

# Store all inputs in a list (but do not print)
markov_inputs <- list(
  health_utilities = health_utilities,
  costs_standard_tmz = costs_standard_tmz,
  costs_longterm_tmz = costs_longterm_tmz,
  model_parameters = model_parameters
)

# Define observed survival probabilities
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
calculate_transition_prob <- function(y, t) {
  1 - exp(log(y) / t)
}

# Number of cycles
num_cycles <- length(OS_standard)

# Initialize transition probability matrices
prob_stable_to_death <- numeric(num_cycles)
prob_stable_to_progressive <- numeric(num_cycles)
prob_progressive_to_death <- numeric(num_cycles)
prob_stable_to_stable <- numeric(num_cycles)
prob_progressive_to_progressive <- numeric(num_cycles)

# Compute transition probabilities for each cycle
t <- 1:num_cycles
prob_stable_to_death <- calculate_transition_prob(OS_standard, t)
prob_stable_to_progressive <- calculate_transition_prob(PFS_standard, t)
prob_progressive_to_death <- calculate_transition_prob(OS_standard - PFS_standard, t)

# Compute stable to stable and progressive to progressive transition probabilities
prob_stable_to_stable <- 1 - (prob_stable_to_progressive + prob_stable_to_death)
prob_progressive_to_progressive <- 1 - prob_progressive_to_death

# Store results in a list
transition_probs_standard <- list(
  stable_to_death = prob_stable_to_death,
  stable_to_progressive = prob_stable_to_progressive,
  progressive_to_death = prob_progressive_to_death,
  stable_to_stable = prob_stable_to_stable,
  progressive_to_progressive = prob_progressive_to_progressive
)

# Define observed survival probabilities for long-term TMZ therapy
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
calculate_transition_prob <- function(y, t) {
  1 - exp(log(y) / t)
}

# Number of cycles
num_cycles <- length(OS_long_term)

# Initialize transition probability matrices
prob_stable_to_death <- numeric(num_cycles)
prob_stable_to_progressive <- numeric(num_cycles)
prob_progressive_to_death <- numeric(num_cycles)
prob_stable_to_stable <- numeric(num_cycles)
prob_progressive_to_progressive <- numeric(num_cycles)

# Compute transition probabilities for each cycle
t <- 1:num_cycles
prob_stable_to_death <- calculate_transition_prob(OS_long_term, t)
prob_stable_to_progressive <- calculate_transition_prob(PFS_long_term, t)
prob_progressive_to_death <- calculate_transition_prob(OS_long_term - PFS_long_term, t)

# Compute stable to stable and progressive to progressive transition probabilities
prob_stable_to_stable <- 1 - (prob_stable_to_progressive + prob_stable_to_death)
prob_progressive_to_progressive <- 1 - prob_progressive_to_death

# Store results in a list
transition_probs_long_term <- list(
  stable_to_death = prob_stable_to_death,
  stable_to_progressive = prob_stable_to_progressive,
  progressive_to_death = prob_progressive_to_death,
  stable_to_stable = prob_stable_to_stable,
  progressive_to_progressive = prob_progressive_to_progressive
)

# Function to compute Markov trace
generate_markov_trace <- function(transition_probs, num_cycles) {
  # Initialize Markov trace
  trace <- matrix(0, nrow = num_cycles, ncol = 3)
  colnames(trace) <- c("Stable", "Progressive", "Death")
  
  # Initial state distribution (all patients start in stable state)
  trace[1, ] <- c(1, 0, 0)
  
  # Compute trace over cycles
  for (t in 2:num_cycles) {
    # Determine which transition probabilities to use
    prob_index <- max(1, t - 2) # Use probabilities from two cycles earlier, or cycle 1 for cycles 2 and 3
    
    # Extract probabilities
    p_stable_to_stable <- transition_probs$stable_to_stable[prob_index]
    p_stable_to_progressive <- transition_probs$stable_to_progressive[prob_index]
    p_stable_to_death <- transition_probs$stable_to_death[prob_index]
    
    p_progressive_to_progressive <- transition_probs$progressive_to_progressive[prob_index]
    p_progressive_to_death <- transition_probs$progressive_to_death[prob_index]
    
    # Apply transitions
    trace[t, "Stable"] <- trace[t-1, "Stable"] * p_stable_to_stable
    trace[t, "Progressive"] <- trace[t-1, "Stable"] * p_stable_to_progressive + trace[t-1, "Progressive"] * p_progressive_to_progressive
    trace[t, "Death"] <- trace[t-1, "Stable"] * p_stable_to_death + trace[t-1, "Progressive"] * p_progressive_to_death + trace[t-1, "Death"]
  }
  
  return(trace)
}

# Define number of cycles
num_cycles <- 60

# Generate Markov traces for both treatments
markov_trace_standard <- generate_markov_trace(transition_probs_standard, num_cycles)
markov_trace_long_term <- generate_markov_trace(transition_probs_long_term, num_cycles)

# Store results in a list
markov_traces <- list(
  standard_tmz = markov_trace_standard,
  long_term_tmz = markov_trace_long_term
)

# Function to calculate QALMs
calculate_qalms <- function(markov_trace, health_utilities, num_cycles) {
  # Initialize QALMs vector
  qalms_per_cycle <- numeric(num_cycles)
  
  # Define utility values
  utility_stable <- health_utilities$stable
  utility_progressive <- health_utilities$progressive
  decrement_per_month <- health_utilities$decrement_per_month
  
  # Generate progressive state utilities with decrement applied
  progressive_utilities <- rep(utility_progressive, num_cycles)
  
  for (t in 3:25) {  # Apply decrement from cycle 3 to cycle 25
    progressive_utilities[t] <- max(0, utility_progressive - ((t - 2) * decrement_per_month))
  }
  
  # Fix the progressive state utility at cycle 25 value for remaining cycles
  progressive_utilities[26:num_cycles] <- progressive_utilities[25]
  
  # Calculate QALMs for each cycle
  for (t in 1:num_cycles) {
    qalms_per_cycle[t] <- (markov_trace[t, "Stable"] * utility_stable) +
      (markov_trace[t, "Progressive"] * progressive_utilities[t])
  }
  
  # Calculate total QALMs
  total_qalms <- sum(qalms_per_cycle)
  
  # Convert total QALMs to total QALYs (dividing by 12)
  total_qalys <- total_qalms / 12
  
  return(list(qalms_per_cycle = qalms_per_cycle, total_qalms = total_qalms, total_qalys = total_qalys))
}

# Calculate QALMs and total QALYs for both treatments
qalms_standard <- calculate_qalms(markov_trace_standard, health_utilities, num_cycles)
qalms_long_term <- calculate_qalms(markov_trace_long_term, health_utilities, num_cycles)

# Store results in a list
qalms_results <- list(
  standard_tmz = qalms_standard,
  long_term_tmz = qalms_long_term
)

# Extract total QALYs for each treatment
total_qalys_standard <- qalms_standard$total_qalys
total_qalys_long_term <- qalms_long_term$total_qalys

# Store total QALYs in a separate list
total_qalys_results <- list(
  standard_tmz = total_qalys_standard,
  long_term_tmz = total_qalys_long_term
)

# Function to calculate costs per cycle
calculate_costs <- function(markov_trace, costs, num_cycles) {
  # Initialize cost vector
  costs_per_cycle <- numeric(num_cycles)
  
  # Define cost categories
  stable_costs <- c(rep(costs$stable[1], 4), rep(costs$stable[4], 3), rep(costs$stable_subsequent, num_cycles - 7))
  progressive_costs <- rep(costs$progressive, num_cycles)
  death_costs <- rep(costs$death, num_cycles)
  
  # Calculate costs for each cycle
  for (t in 1:num_cycles) {
    costs_per_cycle[t] <- (markov_trace[t, "Stable"] * stable_costs[t]) +
      (markov_trace[t, "Progressive"] * progressive_costs[t]) +
      (markov_trace[t, "Death"] * death_costs[t])
  }
  
  # Calculate total costs
  total_costs <- sum(costs_per_cycle)
  
  return(list(costs_per_cycle = costs_per_cycle, total_costs = total_costs))
}

# Calculate costs for both treatments
costs_standard <- calculate_costs(markov_trace_standard, costs_standard_tmz, num_cycles)
costs_long_term <- calculate_costs(markov_trace_long_term, costs_longterm_tmz, num_cycles)

# Store results in a list
costs_results <- list(
  standard_tmz = costs_standard,
  long_term_tmz = costs_long_term
)

# Function to calculate ICER
calculate_icer <- function(costs_high, costs_low, qalms_high, qalms_low, qaly_high, qaly_low) {
  icer_qalms <- round((costs_high - costs_low) / (qalms_high - qalms_low), 0)
  icer_qalys <- round((costs_high - costs_low) / (qaly_high - qaly_low), 0)
  
  return(list(icer_qalms = icer_qalms, icer_qalys = icer_qalys))
}

# Extract cost and effectiveness values
cost_standard <- costs_results$standard_tmz$total_costs
cost_long_term <- costs_results$long_term_tmz$total_costs

qalms_standard <- qalms_results$standard_tmz$total_qalms
qalms_long_term <- qalms_results$long_term_tmz$total_qalms

qaly_standard <- total_qalys_results$standard_tmz
qaly_long_term <- total_qalys_results$long_term_tmz

# Calculate ICER (assuming long-term TMZ is the higher cost option)
icer_results <- calculate_icer(cost_long_term, cost_standard, qalms_long_term, qalms_standard, qaly_long_term, qaly_standard)

# Store results
icer_results
