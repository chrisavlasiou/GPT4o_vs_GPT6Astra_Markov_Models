# Store transition probabilities
tp <- list(
  Standard_Treatment = list(
    stable_to_progressive = 0.0956,
    stable_to_death = 0.0464,
    progressive_to_death = 0.0861
  ),
  Long_Term_Treatment = list(
    stable_to_progressive = 0.0894,
    stable_to_death = 0.0397,
    progressive_to_death = 0.0689
  )
)

# Store health utilities
health_utility <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Store cost data
costs <- list(
  Standard_Treatment = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 350,
    progressive = 2720,
    death = 0
  ),
  Long_Term_Treatment = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 2453,
    progressive = 2720,
    death = 0
  )
)

# Store model design parameters
model_params <- list(
  cycle_length = 1,  # in months
  time_horizon = 60  # in months
)

# Define function to compute Markov trace
compute_markov_trace <- function(tp, cycles) {
  # Initialize Markov trace
  trace <- matrix(0, nrow = cycles, ncol = 3)
  colnames(trace) <- c("Stable", "Progressive", "Death")
  
  # Initial distribution (100% of cohort starts in stable state)
  trace[1, ] <- c(1, 0, 0)
  
  # Transition probabilities
  for (t in 2:cycles) {
    trace[t, 1] <- trace[t - 1, 1] * (1 - tp$stable_to_progressive - tp$stable_to_death)
    trace[t, 2] <- trace[t - 1, 1] * tp$stable_to_progressive + trace[t - 1, 2] * (1 - tp$progressive_to_death)
    trace[t, 3] <- trace[t - 1, 1] * tp$stable_to_death + trace[t - 1, 2] * tp$progressive_to_death + trace[t - 1, 3]
  }
  
  return(trace)
}

# Compute Markov trace for both treatments
markov_trace_standard <- compute_markov_trace(tp$Standard_Treatment, model_params$time_horizon)
markov_trace_long_term <- compute_markov_trace(tp$Long_Term_Treatment, model_params$time_horizon)

# Define function to compute QALMs
compute_qalms <- function(trace, health_utility, cycles) {
  qalms <- numeric(cycles)
  progressive_utility <- health_utility$progressive
  
  for (t in 1:cycles) {
    if (t >= 3 && t <= 25) {
      progressive_utility <- health_utility$progressive - (t - 2) * health_utility$decrement_per_month
    } else if (t > 25) {
      progressive_utility <- health_utility$progressive - (25 - 2) * health_utility$decrement_per_month
    }
    
    qalms[t] <- trace[t, 1] * health_utility$stable + trace[t, 2] * progressive_utility
  }
  
  return(qalms)
}

# Compute QALMs for both treatments
qalms_standard <- compute_qalms(markov_trace_standard, health_utility, model_params$time_horizon)
qalms_long_term <- compute_qalms(markov_trace_long_term, health_utility, model_params$time_horizon)

# Compute total QALMs for both treatments
total_qalms_standard <- sum(qalms_standard)
total_qalms_long_term <- sum(qalms_long_term)

# Convert QALMs to QALYs
total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Define function to compute costs per cycle
compute_costs <- function(trace, costs, cycles) {
  cost_per_cycle <- numeric(cycles)
  
  for (t in 1:cycles) {
    stable_cost <- ifelse(t <= 4, costs$stable_1_3,
                          ifelse(t <= 7, costs$stable_4_6, costs$stable_subsequent))
    
    cost_per_cycle[t] <- trace[t, 1] * stable_cost + trace[t, 2] * costs$progressive + trace[t, 3] * costs$death
  }
  
  return(cost_per_cycle)
}

# Compute costs for both treatments
costs_standard <- compute_costs(markov_trace_standard, costs$Standard_Treatment, model_params$time_horizon)
costs_long_term <- compute_costs(markov_trace_long_term, costs$Long_Term_Treatment, model_params$time_horizon)

# Compute total costs for both treatments
total_costs_standard <- sum(costs_standard)
total_costs_long_term <- sum(costs_long_term)

# Function to compute costs per cycle
compute_costs <- function(trace, costs, cycles) {
  cost_per_cycle <- numeric(cycles)
  
  for (t in 1:cycles) {
    stable_cost <- ifelse(t <= 4, costs$stable_1_3,
                          ifelse(t <= 7, costs$stable_4_6, costs$stable_subsequent))
    
    cost_per_cycle[t] <- trace[t, 1] * stable_cost + trace[t, 2] * costs$progressive + trace[t, 3] * costs$death
  }
  
  return(cost_per_cycle)
}

# Compute costs for both treatments
costs_standard <- compute_costs(markov_trace_standard, costs$Standard_Treatment, model_params$time_horizon)
costs_long_term <- compute_costs(markov_trace_long_term, costs$Long_Term_Treatment, model_params$time_horizon)

# Compute total costs for both treatments
total_costs_standard <- sum(costs_standard)
total_costs_long_term <- sum(costs_long_term)

# Print results
list(
  costs_per_cycle_standard = costs_standard,
  total_costs_standard = total_costs_standard,
  costs_per_cycle_long_term = costs_long_term,
  total_costs_long_term = total_costs_long_term
)

# Function to calculate ICER
compute_icer <- function(costs_treatment1, qalms_treatment1, costs_treatment2, qalms_treatment2) {
  incremental_costs <- costs_treatment2 - costs_treatment1
  incremental_qalms <- qalms_treatment2 - qalms_treatment1
  incremental_qalys <- incremental_qalms / 12
  
  icer_qalms <- round(incremental_costs / incremental_qalms, 0)
  icer_qalys <- round(incremental_costs / incremental_qalys, 0)
  
  return(list(ICER_QALMs = icer_qalms, ICER_QALYs = icer_qalys))
}

# Compute ICER for Long-term TMZ therapy vs. Standard TMZ therapy
icer_results <- compute_icer(
  costs_treatment1 = total_costs_standard, 
  qalms_treatment1 = total_qalms_standard, 
  costs_treatment2 = total_costs_long_term, 
  qalms_treatment2 = total_qalms_long_term
)

# Print ICER results
icer_results
