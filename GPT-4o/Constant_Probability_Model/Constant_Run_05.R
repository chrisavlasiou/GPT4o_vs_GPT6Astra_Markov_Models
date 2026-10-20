# Store transition probabilities
tp <- list(
  standard_tmz = list(
    stable_to_progressive = 0.0956,
    stable_to_death = 0.0464,
    progressive_to_death = 0.0861
  ),
  long_term_tmz = list(
    stable_to_progressive = 0.0894,
    stable_to_death = 0.0397,
    progressive_to_death = 0.0689
  )
)

# Store health utilities
hu <- list(
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

# Define cycle length and time horizon
cycle_length <- 1  # in months
time_horizon <- 60 # in months

# Initialize Markov traces for both treatments
markov_trace <- function(tp, cycles) {
  trace <- matrix(0, nrow = cycles, ncol = 3)
  colnames(trace) <- c("Stable", "Progressive", "Death")
  trace[1, ] <- c(1, 0, 0)  # Initial state distribution
  
  for (t in 2:cycles) {
    trace[t, 1] <- trace[t-1, 1] * (1 - tp$stable_to_progressive - tp$stable_to_death)
    trace[t, 2] <- trace[t-1, 1] * tp$stable_to_progressive + trace[t-1, 2] * (1 - tp$progressive_to_death)
    trace[t, 3] <- trace[t-1, 1] * tp$stable_to_death + trace[t-1, 2] * tp$progressive_to_death + trace[t-1, 3]
  }
  return(trace)
}

# Compute Markov traces for Standard TMZ and Long-term TMZ
trace_standard_tmz <- markov_trace(tp$standard_tmz, time_horizon)
trace_long_term_tmz <- markov_trace(tp$long_term_tmz, time_horizon)

# Store results in a list
markov_results <- list(
  standard_tmz = trace_standard_tmz,
  long_term_tmz = trace_long_term_tmz
)

# Compute QALMs
compute_qalms <- function(trace, hu, cycles) {
  qalms <- numeric(cycles)
  
  for (t in 1:cycles) {
    progressive_utility <- hu$progressive
    
    # Apply decrement for cycles 3 to 25
    if (t >= 3 && t <= 25) {
      progressive_utility <- max(hu$progressive - (t - 2) * hu$decrement_per_month, 0)
    }
    
    # Fix utility after cycle 25
    if (t > 25) {
      progressive_utility <- max(hu$progressive - (25 - 2) * hu$decrement_per_month, 0)
    }
    
    qalms[t] <- trace[t, 1] * hu$stable + trace[t, 2] * progressive_utility
  }
  
  total_qalms <- sum(qalms)
  total_qalys <- total_qalms / 12  # Convert from months to years
  return(list(qalms = qalms, total_qalms = total_qalms, total_qalys = total_qalys))
}

# Calculate QALMs and QALYs for both treatments
qalms_standard_tmz <- compute_qalms(trace_standard_tmz, hu, time_horizon)
qalms_long_term_tmz <- compute_qalms(trace_long_term_tmz, hu, time_horizon)

# Store QALMs and QALYs results
qalms_results <- list(
  standard_tmz = qalms_standard_tmz,
  long_term_tmz = qalms_long_term_tmz
)

# Compute costs
compute_costs <- function(trace, costs, cycles) {
  cost_per_cycle <- numeric(cycles)
  
  for (t in 1:cycles) {
    stable_cost <- ifelse(t <= 4, costs$stable_1_3,
                          ifelse(t <= 7, costs$stable_4_6, costs$stable_subsequent))
    
    cost_per_cycle[t] <- trace[t, 1] * stable_cost + trace[t, 2] * costs$progressive + trace[t, 3] * costs$death
  }
  
  total_cost <- sum(cost_per_cycle)
  return(list(costs = cost_per_cycle, total_cost = total_cost))
}

# Calculate costs for both treatments
costs_standard_tmz <- compute_costs(trace_standard_tmz, costs$standard_tmz, time_horizon)
costs_long_term_tmz <- compute_costs(trace_long_term_tmz, costs$long_term_tmz, time_horizon)

# Store costs results
costs_results <- list(
  standard_tmz = costs_standard_tmz,
  long_term_tmz = costs_long_term_tmz
)

# Compute ICERs
icer_qalms <- round((costs_results$long_term_tmz$total_cost - costs_results$standard_tmz$total_cost) /
                      (qalms_results$long_term_tmz$total_qalms - qalms_results$standard_tmz$total_qalms))

icer_qalys <- round((costs_results$long_term_tmz$total_cost - costs_results$standard_tmz$total_cost) /
                      (qalms_results$long_term_tmz$total_qalys - qalms_results$standard_tmz$total_qalys))

# Store ICER results
icer_results <- list(
  icer_qalms = nicer_qalms,
  icer_qalys = nicer_qalys
)
