# Store transition probabilities
tp <- list(
  Standard_TMT = list(
    stable_to_progressive = 0.0956,
    stable_to_death = 0.0464,
    progressive_to_death = 0.0861
  ),
  Long_TMT = list(
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

# Store costs
costs <- list(
  Standard_TMT = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 350,
    progressive = 2720,
    death = 0
  ),
  Long_TMT = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 2453,
    progressive = 2720,
    death = 0
  )
)

# Define cycle length and time horizon
cycle_length <- 1  # in months
time_horizon <- 60  # in months

# Initialize Markov trace
define_markov_trace <- function(tp) {
  trace <- matrix(0, nrow = time_horizon, ncol = 3)
  colnames(trace) <- c("Stable", "Progressive", "Death")
  
  # Initial state: All patients start in Stable
  trace[1, ] <- c(1, 0, 0)
  
  for (t in 2:time_horizon) {
    trace[t, 1] <- trace[t - 1, 1] * (1 - tp$stable_to_progressive - tp$stable_to_death)
    trace[t, 2] <- trace[t - 1, 1] * tp$stable_to_progressive + trace[t - 1, 2] * (1 - tp$progressive_to_death)
    trace[t, 3] <- trace[t - 1, 1] * tp$stable_to_death + trace[t - 1, 2] * tp$progressive_to_death + trace[t - 1, 3]
  }
  
  return(trace)
}

# Compute Markov traces
markov_trace_standard <- define_markov_trace(tp$Standard_TMT)
markov_trace_long <- define_markov_trace(tp$Long_TMT)

# Compute QALMs
compute_qalms <- function(trace) {
  qalms <- numeric(time_horizon)
  progressive_utility <- health_utility$progressive
  
  for (t in 1:time_horizon) {
    if (t >= 3 && t <= 25) {
      progressive_utility <- max(health_utility$progressive - (t - 2) * health_utility$decrement_per_month, 0)
    }
    
    qalms[t] <- trace[t, 1] * health_utility$stable + trace[t, 2] * progressive_utility
  }
  
  total_qalms <- sum(qalms)
  total_qalys <- total_qalms / 12  # Convert QALMs to QALYs
  
  return(list(qalms_per_cycle = qalms, total_qalms = total_qalms, total_qalys = total_qalys))
}

# Calculate QALMs and QALYs for both treatments
qalms_standard <- compute_qalms(markov_trace_standard)
qalms_long <- compute_qalms(markov_trace_long)

# Compute costs
compute_costs <- function(trace, cost_scheme) {
  costs_per_cycle <- numeric(time_horizon)
  
  for (t in 1:time_horizon) {
    if (t <= 4) {
      stable_cost <- cost_scheme$stable_1_3
    } else if (t <= 7) {
      stable_cost <- cost_scheme$stable_4_6
    } else {
      stable_cost <- cost_scheme$stable_subsequent
    }
    
    costs_per_cycle[t] <- trace[t, 1] * stable_cost + trace[t, 2] * cost_scheme$progressive + trace[t, 3] * cost_scheme$death
  }
  
  total_cost <- sum(costs_per_cycle)
  
  return(list(costs_per_cycle = costs_per_cycle, total_cost = total_cost))
}

# Calculate costs for both treatments
costs_standard <- compute_costs(markov_trace_standard, costs$Standard_TMT)
costs_long <- compute_costs(markov_trace_long, costs$Long_TMT)

# Compute ICER
incremental_cost <- costs_long$total_cost - costs_standard$total_cost
incremental_qalms <- qalms_long$total_qalms - qalms_standard$total_qalms
incremental_qalys <- qalms_long$total_qalys - qalms_standard$total_qalys

icer_qalms <- round(incremental_cost / incremental_qalms, 0)
icer_qalys <- round(incremental_cost / incremental_qalys, 0)

# Store results
results <- list(
  Standard_TMT = list(trace = markov_trace_standard, qalms = qalms_standard, costs = costs_standard),
  Long_TMT = list(trace = markov_trace_long, qalms = qalms_long, costs = costs_long),
  ICER = list(icer_qalms = icer_qalms, icer_qalys = icer_qalys)
)
