# Transition probabilities
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

# Health utilities
health_utility <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Costs
treatment_costs <- list(
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

# Model parameters
model_params <- list(
  cycle_length = 1,  # in months
  time_horizon = 60  # in months
)

# Markov trace calculation
calculate_markov_trace <- function(tp, time_horizon) {
  trace <- matrix(0, nrow = time_horizon, ncol = 3)
  colnames(trace) <- c("Stable", "Progressive", "Death")
  
  # Initial state: 100% in Stable
  trace[1, ] <- c(1, 0, 0)
  
  for (t in 2:time_horizon) {
    trace[t, 1] <- trace[t - 1, 1] * (1 - tp$stable_to_progressive - tp$stable_to_death)
    trace[t, 2] <- trace[t - 1, 1] * tp$stable_to_progressive + trace[t - 1, 2] * (1 - tp$progressive_to_death)
    trace[t, 3] <- trace[t - 1, 1] * tp$stable_to_death + trace[t - 1, 2] * tp$progressive_to_death + trace[t - 1, 3]
  }
  
  return(trace)
}

# Calculate Markov traces for both treatments
markov_trace_standard <- calculate_markov_trace(tp$standard_tmz, model_params$time_horizon)
markov_trace_long_term <- calculate_markov_trace(tp$long_term_tmz, model_params$time_horizon)

# Calculate QALMs
calculate_qalms <- function(trace, health_utility, time_horizon) {
  qalms <- numeric(time_horizon)
  progressive_utility <- health_utility$progressive
  
  for (t in 1:time_horizon) {
    if (t >= 3 && t <= 25) {
      decrement <- (t - 2) * health_utility$decrement_per_month
      progressive_utility <- max(health_utility$progressive - decrement, health_utility$progressive - (23 * health_utility$decrement_per_month))
    } else if (t > 25) {
      progressive_utility <- health_utility$progressive - (23 * health_utility$decrement_per_month)
    }
    
    qalms[t] <- trace[t, 1] * health_utility$stable + trace[t, 2] * progressive_utility
  }
  
  total_qalms <- sum(qalms)
  return(list(qalms = qalms, total_qalms = total_qalms))
}

# Compute QALMs for both treatments
qalms_standard <- calculate_qalms(markov_trace_standard, health_utility, model_params$time_horizon)
qalms_long_term <- calculate_qalms(markov_trace_long_term, health_utility, model_params$time_horizon)

# Store QALMs results
qalms_results <- list(
  standard_tmz = qalms_standard,
  long_term_tmz = qalms_long_term
)

# Calculate total QALYs
total_qalys_standard <- qalms_standard$total_qalms / 12
total_qalys_long_term <- qalms_long_term$total_qalms / 12

# Store total QALYs
qalys_results <- list(
  standard_tmz = total_qalys_standard,
  long_term_tmz = total_qalys_long_term
)

# Calculate cost per cycle
calculate_costs <- function(trace, costs, time_horizon) {
  total_costs <- numeric(time_horizon)
  
  for (t in 1:time_horizon) {
    stable_cost <- ifelse(t <= 4, costs$stable_1_3, ifelse(t <= 7, costs$stable_4_6, costs$stable_subsequent))
    total_costs[t] <- trace[t, 1] * stable_cost + trace[t, 2] * costs$progressive + trace[t, 3] * costs$death
  }
  
  total_cost <- sum(total_costs)
  return(list(costs_per_cycle = total_costs, total_cost = total_cost))
}

# Compute costs for both treatments
costs_standard <- calculate_costs(markov_trace_standard, treatment_costs$standard_tmz, model_params$time_horizon)
costs_long_term <- calculate_costs(markov_trace_long_term, treatment_costs$long_term_tmz, model_params$time_horizon)

# Store cost results
costs_results <- list(
  standard_tmz = costs_standard,
  long_term_tmz = costs_long_term
)

# Calculate ICERs
incremental_cost <- costs_long_term$total_cost - costs_standard$total_cost
incremental_qalms <- qalms_results$long_term_tmz$total_qalms - qalms_results$standard_tmz$total_qalms
incremental_qalys <- qalys_results$long_term_tmz - qalys_results$standard_tmz

icer_qalms <- round(incremental_cost / incremental_qalms, 0)
icer_qalys <- round(incremental_cost / incremental_qalys, 0)

# Store ICER results
icer_results <- list(
  qalms = icer_qalms,
  qalys = icer_qalys
)
