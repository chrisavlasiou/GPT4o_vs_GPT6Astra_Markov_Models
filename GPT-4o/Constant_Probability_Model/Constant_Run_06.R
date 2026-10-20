# Store transition probabilities
transition_probs <- list(
  standard_TMZ = list(
    stable_to_progressive = 0.0956,
    stable_to_death = 0.0464,
    progressive_to_death = 0.0861
  ),
  long_term_TMZ = list(
    stable_to_progressive = 0.0894,
    stable_to_death = 0.0397,
    progressive_to_death = 0.0689
  )
)

# Store health utility data
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Store cost data
costs <- list(
  standard_TMZ = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 350,
    progressive = 2720,
    death = 0
  ),
  long_term_TMZ = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 2453,
    progressive = 2720,
    death = 0
  )
)

# Define model time parameters
cycle_length <- 1  # months
time_horizon <- 60 # months

# Function to calculate Markov trace
calculate_markov_trace <- function(transitions, cycles) {
  trace <- matrix(0, nrow = cycles, ncol = 3)
  colnames(trace) <- c("Stable", "Progressive", "Death")
  trace[1, ] <- c(1, 0, 0)  # Initial state: all patients in stable state
  
  for (t in 2:cycles) {
    trace[t, 1] <- trace[t-1, 1] * (1 - transitions$stable_to_progressive - transitions$stable_to_death)
    trace[t, 2] <- trace[t-1, 1] * transitions$stable_to_progressive + trace[t-1, 2] * (1 - transitions$progressive_to_death)
    trace[t, 3] <- trace[t-1, 1] * transitions$stable_to_death + trace[t-1, 2] * transitions$progressive_to_death + trace[t-1, 3]
  }
  return(trace)
}

# Calculate Markov trace for both treatments
markov_trace_standard <- calculate_markov_trace(transition_probs$standard_TMZ, time_horizon)
markov_trace_long_term <- calculate_markov_trace(transition_probs$long_term_TMZ, time_horizon)

# Function to calculate QALMs
calculate_QALMs <- function(trace, utilities, cycles) {
  QALMs <- numeric(cycles)
  progressive_utilities <- rep(utilities$progressive, cycles)
  
  # Apply decrement for progressive state from cycle 3 to cycle 25
  for (t in 3:25) {
    progressive_utilities[t] <- max(utilities$progressive - (t - 2) * utilities$decrement_per_month, 0)
  }
  # Fix utility at cycle 25 value for all subsequent cycles
  progressive_utilities[26:cycles] <- progressive_utilities[25]
  
  for (t in 1:cycles) {
    QALMs[t] <- trace[t, 1] * utilities$stable + trace[t, 2] * progressive_utilities[t]
  }
  
  total_QALMs <- sum(QALMs)
  return(list(QALMs = QALMs, total_QALMs = total_QALMs))
}

# Calculate QALMs for both treatments
QALMs_standard <- calculate_QALMs(markov_trace_standard, health_utilities, time_horizon)
QALMs_long_term <- calculate_QALMs(markov_trace_long_term, health_utilities, time_horizon)

# Convert total QALMs to total QALYs
QALYs_standard <- QALMs_standard$total_QALMs / 12
QALYs_long_term <- QALMs_long_term$total_QALMs / 12

# Function to calculate costs
calculate_costs <- function(trace, cost_data, cycles) {
  costs_per_cycle <- numeric(cycles)
  
  for (t in 1:cycles) {
    stable_cost <- ifelse(t <= 4, cost_data$stable_1_3,
                          ifelse(t <= 7, cost_data$stable_4_6,
                                 cost_data$stable_subsequent))
    costs_per_cycle[t] <- trace[t, 1] * stable_cost + trace[t, 2] * cost_data$progressive + trace[t, 3] * cost_data$death
  }
  
  total_costs <- sum(costs_per_cycle)
  return(list(costs_per_cycle = costs_per_cycle, total_costs = total_costs))
}

# Calculate costs for both treatments
costs_standard <- calculate_costs(markov_trace_standard, costs$standard_TMZ, time_horizon)
costs_long_term <- calculate_costs(markov_trace_long_term, costs$long_term_TMZ, time_horizon)

# Calculate ICERs
incremental_cost <- costs_long_term$total_costs - costs_standard$total_costs
incremental_QALMs <- QALMs_long_term$total_QALMs - QALMs_standard$total_QALMs
incremental_QALYs <- QALYs_long_term - QALYs_standard

ICER_QALMs <- round(incremental_cost / incremental_QALMs, 0)
ICER_QALYs <- round(incremental_cost / incremental_QALYs, 0)
