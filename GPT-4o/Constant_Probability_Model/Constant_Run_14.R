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

# Store health utility data
hu <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Store costs
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

# Store cycle length and time horizon
model_parameters <- list(
  cycle_length = 1,  # in months
  time_horizon = 60  # in months
)

# Initialize Markov trace function
markov_trace <- function(tp, cycles) {
  trace <- matrix(0, nrow = cycles, ncol = 3)
  colnames(trace) <- c("Stable", "Progressive", "Death")
  trace[1, ] <- c(1, 0, 0)  # Initial state: all patients start in Stable
  
  for (t in 2:cycles) {
    trace[t, 1] <- trace[t - 1, 1] * (1 - tp$stable_to_progressive - tp$stable_to_death)
    trace[t, 2] <- trace[t - 1, 1] * tp$stable_to_progressive + trace[t - 1, 2] * (1 - tp$progressive_to_death)
    trace[t, 3] <- trace[t - 1, 1] * tp$stable_to_death + trace[t - 1, 2] * tp$progressive_to_death + trace[t - 1, 3]
  }
  
  return(trace)
}

# Calculate Markov trace for both treatments
trace_standard_tmz <- markov_trace(tp$standard_tmz, model_parameters$time_horizon)
trace_long_term_tmz <- markov_trace(tp$long_term_tmz, model_parameters$time_horizon)

# Calculate QALMs
calculate_qalms <- function(trace, hu, cycles) {
  qalms <- numeric(cycles)
  progressive_utility <- hu$progressive
  
  for (t in 1:cycles) {
    if (t >= 3 && t <= 25) {
      progressive_utility <- max(hu$progressive - (t - 2) * hu$decrement_per_month, hu$progressive - (25 - 2) * hu$decrement_per_month)
    }
    
    qalms[t] <- trace[t, 1] * hu$stable + trace[t, 2] * progressive_utility
  }
  
  total_qalms <- sum(qalms)
  return(list(qalms = qalms, total_qalms = total_qalms))
}

# Compute QALMs for both treatments
qalms_standard_tmz <- calculate_qalms(trace_standard_tmz, hu, model_parameters$time_horizon)
qalms_long_term_tmz <- calculate_qalms(trace_long_term_tmz, hu, model_parameters$time_horizon)

# Convert total QALMs to QALYs
total_qalys_standard_tmz <- qalms_standard_tmz$total_qalms / 12
total_qalys_long_term_tmz <- qalms_long_term_tmz$total_qalms / 12

# Calculate costs
calculate_costs <- function(trace, costs, cycles) {
  cost_per_cycle <- numeric(cycles)
  
  for (t in 1:cycles) {
    if (t <= 4) {
      stable_cost <- costs$stable_1_3
    } else if (t <= 7) {
      stable_cost <- costs$stable_4_6
    } else {
      stable_cost <- costs$stable_subsequent
    }
    
    cost_per_cycle[t] <- trace[t, 1] * stable_cost + trace[t, 2] * costs$progressive + trace[t, 3] * costs$death
  }
  
  total_cost <- sum(cost_per_cycle)
  return(list(cost_per_cycle = cost_per_cycle, total_cost = total_cost))
}

# Compute costs for both treatments
costs_standard_tmz <- calculate_costs(trace_standard_tmz, treatment_costs$standard_tmz, model_parameters$time_horizon)
costs_long_term_tmz <- calculate_costs(trace_long_term_tmz, treatment_costs$long_term_tmz, model_parameters$time_horizon)

# Calculate ICERs
incremental_cost <- costs_long_term_tmz$total_cost - costs_standard_tmz$total_cost
incremental_qalms <- qalms_long_term_tmz$total_qalms - qalms_standard_tmz$total_qalms
incremental_qalys <- total_qalys_long_term_tmz - total_qalys_standard_tmz

icer_qalms <- round(incremental_cost / incremental_qalms, 0)
icer_qalys <- round(incremental_cost / incremental_qalys, 0)
