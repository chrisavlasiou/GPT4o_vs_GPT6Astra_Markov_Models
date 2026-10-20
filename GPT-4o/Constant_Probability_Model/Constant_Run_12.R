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

# Store health utility values
health_utility <- list(
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

# Define model parameters
cycle_length <- 1  # in months
time_horizon <- 60  # in months

# Initialize Markov trace
markov_trace <- function(tp, cycles) {
  trace <- matrix(0, nrow = cycles, ncol = 3) # Columns: Stable, Progressive, Death
  colnames(trace) <- c("Stable", "Progressive", "Death")
  trace[1,] <- c(1, 0, 0) # Initial state: all in stable
  
  for (t in 2:cycles) {
    trace[t, 1] <- trace[t-1, 1] * (1 - tp$stable_to_progressive - tp$stable_to_death)
    trace[t, 2] <- trace[t-1, 1] * tp$stable_to_progressive + trace[t-1, 2] * (1 - tp$progressive_to_death)
    trace[t, 3] <- trace[t-1, 1] * tp$stable_to_death + trace[t-1, 2] * tp$progressive_to_death + trace[t-1, 3]
  }
  
  return(trace)
}

# Compute Markov traces
trace_standard_tmz <- markov_trace(tp$standard_tmz, time_horizon)
trace_long_term_tmz <- markov_trace(tp$long_term_tmz, time_horizon)

# Store results in a list
markov_traces <- list(
  standard_tmz = trace_standard_tmz,
  long_term_tmz = trace_long_term_tmz
)

# Calculate QALMs per cycle
calculate_qalms <- function(trace, health_utility, cycles) {
  qalms <- numeric(cycles)
  
  for (t in 1:cycles) {
    progressive_utility <- health_utility$progressive
    if (t >= 3 && t <= 25) {
      progressive_utility <- max(health_utility$progressive - (t - 2) * health_utility$decrement_per_month, 0)
    } else if (t > 25) {
      progressive_utility <- max(health_utility$progressive - (25 - 2) * health_utility$decrement_per_month, 0)
    }
    
    qalms[t] <- trace[t, 1] * health_utility$stable + trace[t, 2] * progressive_utility
  }
  
  return(qalms)
}

# Compute QALMs
qalms_standard_tmz <- calculate_qalms(trace_standard_tmz, health_utility, time_horizon)
qalms_long_term_tmz <- calculate_qalms(trace_long_term_tmz, health_utility, time_horizon)

# Compute total QALMs
total_qalms_standard_tmz <- sum(qalms_standard_tmz)
total_qalms_long_term_tmz <- sum(qalms_long_term_tmz)

# Compute total QALYs
total_qalys_standard_tmz <- total_qalms_standard_tmz / 12
total_qalys_long_term_tmz <- total_qalms_long_term_tmz / 12

# Calculate costs per cycle
calculate_costs <- function(trace, costs, cycles) {
  cost_per_cycle <- numeric(cycles)
  
  for (t in 1:cycles) {
    stable_cost <- ifelse(t <= 4, costs$stable_1_3,
                          ifelse(t <= 7, costs$stable_4_6, costs$stable_subsequent))
    
    cost_per_cycle[t] <- trace[t, 1] * stable_cost + trace[t, 2] * costs$progressive + trace[t, 3] * costs$death
  }
  
  return(cost_per_cycle)
}

# Compute costs
costs_standard_tmz <- calculate_costs(trace_standard_tmz, costs$standard_tmz, time_horizon)
costs_long_term_tmz <- calculate_costs(trace_long_term_tmz, costs$long_term_tmz, time_horizon)

# Compute total costs
total_cost_standard_tmz <- sum(costs_standard_tmz)
total_cost_long_term_tmz <- sum(costs_long_term_tmz)

# Compute ICERs
incremental_cost <- total_cost_long_term_tmz - total_cost_standard_tmz
incremental_qalms <- total_qalms_long_term_tmz - total_qalms_standard_tmz
incremental_qalys <- total_qalys_long_term_tmz - total_qalys_standard_tmz

icer_qalms <- round(incremental_cost / incremental_qalms, 0)
icer_qalys <- round(incremental_cost / incremental_qalys, 0)

# Store results
results <- list(
  standard_tmz = list(
    per_cycle_costs = costs_standard_tmz,
    total_cost = total_cost_standard_tmz,
    total_qalms = total_qalms_standard_tmz,
    total_qalys = total_qalys_standard_tmz
  ),
  long_term_tmz = list(
    per_cycle_costs = costs_long_term_tmz,
    total_cost = total_cost_long_term_tmz,
    total_qalms = total_qalms_long_term_tmz,
    total_qalys = total_qalys_long_term_tmz
  ),
  ICERs = list(
    ICER_QALMs = icer_qalms,
    ICER_QALYs = icer_qalys
  )
)
