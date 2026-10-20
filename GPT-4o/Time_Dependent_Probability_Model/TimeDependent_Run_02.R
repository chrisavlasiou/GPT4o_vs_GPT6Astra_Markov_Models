# Store health utility data
health_utility <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Store costs data
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

# Define the observed OS and PFS data for Standard TMZ therapy
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
calculate_transition_probability <- function(y, t) {
  return(1 - exp(log(y) / t))
}

# Initialize transition probability matrices
transition_probs_standard <- data.frame(
  cycle = 1:60,
  stable_to_death = sapply(1:60, function(t) calculate_transition_probability(OS_standard[t], t)),
  stable_to_progressive = sapply(1:60, function(t) calculate_transition_probability(PFS_standard[t], t)),
  progressive_to_death = sapply(1:60, function(t) calculate_transition_probability(OS_standard[t] - PFS_standard[t], t))
)

# Calculate stable to stable and progressive to progressive probabilities
transition_probs_standard$stable_to_stable <- 1 - (transition_probs_standard$stable_to_death + transition_probs_standard$stable_to_progressive)
transition_probs_standard$progressive_to_progressive <- 1 - transition_probs_standard$progressive_to_death

# Store transition probabilities
transition_probs_standard

# Define the observed OS and PFS data for Long-term TMZ therapy
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
calculate_transition_probability <- function(y, t) {
  return(1 - exp(log(y) / t))
}

# Initialize transition probability matrices
transition_probs_long_term <- data.frame(
  cycle = 1:60,
  stable_to_death = sapply(1:60, function(t) calculate_transition_probability(OS_long_term[t], t)),
  stable_to_progressive = sapply(1:60, function(t) calculate_transition_probability(PFS_long_term[t], t)),
  progressive_to_death = sapply(1:60, function(t) calculate_transition_probability(OS_long_term[t] - PFS_long_term[t], t))
)

# Calculate stable to stable and progressive to progressive probabilities
transition_probs_long_term$stable_to_stable <- 1 - (transition_probs_long_term$stable_to_death + transition_probs_long_term$stable_to_progressive)
transition_probs_long_term$progressive_to_progressive <- 1 - transition_probs_long_term$progressive_to_death

# Store transition probabilities
transition_probs_long_term

# Function to compute Markov trace
compute_markov_trace <- function(transition_probs, cycles = 60) {
  trace <- matrix(0, nrow = cycles, ncol = 3)
  colnames(trace) <- c("Stable", "Progressive", "Death")
  
  # Initial state distribution
  trace[1, ] <- c(1, 0, 0) # 100% of patients start in Stable
  
  for (t in 2:cycles) {
    # Select the correct transition probabilities
    if (t <= 3) {
      tp <- transition_probs[1, ]  # Use cycle 1 probabilities for cycles 2 and 3
    } else {
      tp <- transition_probs[t - 2, ]  # Use probabilities from cycle (t-2)
    }
    
    # Apply transitions
    trace[t, 1] <- trace[t - 1, 1] * tp$stable_to_stable
    trace[t, 2] <- trace[t - 1, 1] * tp$stable_to_progressive + trace[t - 1, 2] * tp$progressive_to_progressive
    trace[t, 3] <- trace[t - 1, 1] * tp$stable_to_death + trace[t - 1, 2] * tp$progressive_to_death + trace[t - 1, 3]
  }
  
  return(trace)
}

# Compute Markov traces for both treatments
markov_trace_standard <- compute_markov_trace(transition_probs_standard, cycles = 60)
markov_trace_long_term <- compute_markov_trace(transition_probs_long_term, cycles = 60)

# Store the results
markov_trace_standard
markov_trace_long_term

# Function to calculate QALMs
calculate_qalms <- function(markov_trace, health_utility, cycles = 60) {
  qalms <- numeric(cycles)
  
  # Initialize progressive state utility adjustments
  progressive_utilities <- rep(health_utility$progressive, cycles)
  
  # Apply decrement from cycle 3 to cycle 25
  for (t in 3:25) {
    progressive_utilities[t] <- health_utility$progressive - (0.02 * (t - 2))
  }
  
  # After cycle 25, maintain the last decremented utility value
  progressive_utilities[26:cycles] <- progressive_utilities[25]
  
  # Calculate QALMs per cycle
  for (t in 1:cycles) {
    qalms[t] <- (markov_trace[t, "Stable"] * health_utility$stable) +
      (markov_trace[t, "Progressive"] * progressive_utilities[t])
  }
  
  # Total QALMs
  total_qalms <- sum(qalms)
  
  # Convert to QALYs (divide by 12 since QALMs are monthly)
  total_qalys <- total_qalms / 12
  
  return(list(qalms_per_cycle = qalms, total_qalms = total_qalms, total_qalys = total_qalys))
}

# Compute QALMs and QALYs for both treatments
qalms_standard <- calculate_qalms(markov_trace_standard, health_utility, cycles = 60)
qalms_long_term <- calculate_qalms(markov_trace_long_term, health_utility, cycles = 60)

# Store the results
qalms_standard
total_qalys_standard <- qalms_standard$total_qalys
qalms_long_term
total_qalys_long_term <- qalms_long_term$total_qalys

# Output total QALYs
list(total_qalys_standard = total_qalys_standard, total_qalys_long_term = total_qalys_long_term)

# Function to calculate costs per cycle
calculate_costs <- function(markov_trace, costs, cycles = 60) {
  total_costs <- numeric(cycles)
  
  # Define cost structure by cycle range
  cost_structure <- c(rep(costs$stable_1_3, 4),  # Cycles 1-4 (Months 1-3)
                      rep(costs$stable_4_6, 3),  # Cycles 5-7 (Months 4-6)
                      rep(costs$stable_subsequent, 53))  # Cycles 8-60 (Subsequent months)
  
  # Calculate costs per cycle
  for (t in 1:cycles) {
    total_costs[t] <- (markov_trace[t, "Stable"] * cost_structure[t]) +
      (markov_trace[t, "Progressive"] * costs$progressive) +
      (markov_trace[t, "Death"] * costs$death)
  }
  
  # Total costs over all cycles
  total_cost <- sum(total_costs)
  
  return(list(costs_per_cycle = total_costs, total_cost = total_cost))
}

# Compute costs for both treatments
costs_standard <- calculate_costs(markov_trace_standard, costs$standard_tmz, cycles = 60)
costs_long_term <- calculate_costs(markov_trace_long_term, costs$long_term_tmz, cycles = 60)

# Store the results
costs_standard
total_cost_standard <- costs_standard$total_cost
costs_long_term
total_cost_long_term <- costs_long_term$total_cost

# Output total costs
list(total_cost_standard = total_cost_standard, total_cost_long_term = total_cost_long_term)

# Function to calculate ICER
calculate_icer <- function(costs_treatment1, costs_treatment2, qalms_treatment1, qalms_treatment2) {
  
  # Incremental cost
  incremental_cost <- costs_treatment2 - costs_treatment1
  
  # Incremental QALMs
  incremental_qalms <- qalms_treatment2 - qalms_treatment1
  
  # Incremental QALYs (QALMs converted to years by dividing by 12)
  incremental_qalys <- incremental_qalms / 12
  
  # Calculate ICER for QALMs and QALYs
  icer_qalms <- round(incremental_cost / incremental_qalms)
  icer_qalys <- round(incremental_cost / incremental_qalys)
  
  return(list(ICER_QALMs = icer_qalms, ICER_QALYs = icer_qalys))
}

# Compute ICER for both QALMs and QALYs
icer_results <- calculate_icer(total_cost_standard, total_cost_long_term, 
                               total_qalys_standard * 12, total_qalys_long_term * 12)

# Output ICER results
icer_results
