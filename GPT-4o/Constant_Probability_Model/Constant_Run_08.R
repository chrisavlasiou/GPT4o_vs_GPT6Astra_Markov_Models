# Store transition probabilities
transition_probabilities <- list(
  Standard_TMC = list(
    stable_to_progressive = 0.0956,
    stable_to_death = 0.0464,
    progressive_to_death = 0.0861
  ),
  Long_term_TMC = list(
    stable_to_progressive = 0.0894,
    stable_to_death = 0.0397,
    progressive_to_death = 0.0689
  )
)

# Store health utilities
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  utility_decrement_progression = 0.02
)

# Store cost data
costs <- list(
  Standard_TMC = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 350,
    progressive = 2720,
    death = 0
  ),
  Long_term_TMC = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 2453,
    progressive = 2720,
    death = 0
  )
)

# Define cycle length and time horizon
cycle_length <- 1  # 1 month
time_horizon <- 60 # 60 months

# Define Markov Model parameters
cycles <- 60  # Total number of cycles

# Initial state distribution (all patients start in stable state)
initial_distribution <- c(stable = 1, progressive = 0, death = 0)

# Transition probability matrices for each treatment
transition_matrix <- list(
  Standard_TMC = matrix(c(
    1 - (0.0956 + 0.0464), 0.0956, 0.0464,  # Stable state transitions
    0, 1 - 0.0861, 0.0861,                 # Progressive state transitions
    0, 0, 1                                 # Death state (absorbing state)
  ), nrow = 3, byrow = TRUE),
  
  Long_term_TMC = matrix(c(
    1 - (0.0894 + 0.0397), 0.0894, 0.0397,  # Stable state transitions
    0, 1 - 0.0689, 0.0689,                 # Progressive state transitions
    0, 0, 1                                 # Death state (absorbing state)
  ), nrow = 3, byrow = TRUE)
)

# Function to calculate Markov trace
calculate_markov_trace <- function(transition_matrix, cycles) {
  trace <- matrix(0, nrow = cycles, ncol = 3)
  colnames(trace) <- c("Stable", "Progressive", "Death")
  trace[1, ] <- initial_distribution
  
  for (t in 2:cycles) {
    trace[t, ] <- trace[t - 1, ] %*% transition_matrix
  }
  
  return(trace)
}

# Compute Markov traces for both treatments
trace_standard <- calculate_markov_trace(transition_matrix$Standard_TMC, cycles)
trace_long_term <- calculate_markov_trace(transition_matrix$Long_term_TMC, cycles)

# Store results in a list
markov_traces <- list(
  Standard_TMC = trace_standard,
  Long_term_TMC = trace_long_term
)

# Output the results
print("Markov trace for Standard TMZ therapy:")
print(trace_standard)

print("Markov trace for Long-term TMZ therapy:")
print(trace_long_term)

# Define health utilities
stable_utility <- health_utilities$stable
progressive_utility <- health_utilities$progressive
utility_decrement <- health_utilities$utility_decrement_progression

# Initialize vectors to store QALMs per cycle
qalms_standard <- numeric(cycles)
qalms_long_term <- numeric(cycles)

# Compute QALMs for each cycle
for (t in 1:cycles) {
  # Calculate the decremented utility for progressive state
  if (t >= 3 && t <= 25) {
    decremented_utility <- max(progressive_utility - (t - 2) * utility_decrement, 0)  # Ensure it doesn't go below 0
  } else if (t > 25) {
    decremented_utility <- max(progressive_utility - (25 - 2) * utility_decrement, 0)  # Fixed after cycle 25
  } else {
    decremented_utility <- progressive_utility  # No decrement before cycle 3
  }
  
  # Calculate QALMs for Standard TMZ therapy
  qalms_standard[t] <- (trace_standard[t, "Stable"] * stable_utility) +
    (trace_standard[t, "Progressive"] * decremented_utility)
  
  # Calculate QALMs for Long-term TMZ therapy
  qalms_long_term[t] <- (trace_long_term[t, "Stable"] * stable_utility) +
    (trace_long_term[t, "Progressive"] * decremented_utility)
}

# Compute total QALMs over all cycles
total_qalms_standard <- sum(qalms_standard)
total_qalms_long_term <- sum(qalms_long_term)

# Store results in a data frame for clarity
qalms_results <- data.frame(
  Cycle = 1:cycles,
  QALMs_Standard_TMC = qalms_standard,
  QALMs_Long_Term_TMC = qalms_long_term
)

# Display results
print("QALMs per cycle:")
print(qalms_results)

print(paste("Total QALMs for Standard TMZ Therapy:", round(total_qalms_standard, 4)))
print(paste("Total QALMs for Long-term TMZ Therapy:", round(total_qalms_long_term, 4)))

# Convert total QALMs to QALYs
total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Print results
print(paste("Total QALYs for Standard TMZ Therapy:", round(total_qalys_standard, 4)))
print(paste("Total QALYs for Long-term TMZ Therapy:", round(total_qalys_long_term, 4)))

# Initialize cost vectors
costs_standard <- numeric(cycles)
costs_long_term <- numeric(cycles)

# Compute costs for each cycle
for (t in 1:cycles) {
  # Determine cost category based on cycle number
  if (t <= 4) {  # Cycles 1-4 correspond to months 1-3
    cost_stable_standard <- costs$Standard_TMC$stable_1_3
    cost_stable_long_term <- costs$Long_term_TMC$stable_1_3
  } else if (t <= 7) {  # Cycles 5-7 correspond to months 4-6
    cost_stable_standard <- costs$Standard_TMC$stable_4_6
    cost_stable_long_term <- costs$Long_term_TMC$stable_4_6
  } else {  # Cycles 8-60 correspond to subsequent months
    cost_stable_standard <- costs$Standard_TMC$stable_subsequent
    cost_stable_long_term <- costs$Long_term_TMC$stable_subsequent
  }
  
  # Calculate costs for each treatment
  costs_standard[t] <- (trace_standard[t, "Stable"] * cost_stable_standard) +
    (trace_standard[t, "Progressive"] * costs$Standard_TMC$progressive) +
    (trace_standard[t, "Death"] * costs$Standard_TMC$death)
  
  costs_long_term[t] <- (trace_long_term[t, "Stable"] * cost_stable_long_term) +
    (trace_long_term[t, "Progressive"] * costs$Long_term_TMC$progressive) +
    (trace_long_term[t, "Death"] * costs$Long_term_TMC$death)
}

# Compute total costs over all cycles
total_costs_standard <- sum(costs_standard)
total_costs_long_term <- sum(costs_long_term)

# Store results in a data frame for clarity
costs_results <- data.frame(
  Cycle = 1:cycles,
  Costs_Standard_TMC = costs_standard,
  Costs_Long_Term_TMC = costs_long_term
)

# Display results
print("Costs per cycle:")
print(costs_results)

print(paste("Total Costs for Standard TMZ Therapy:", round(total_costs_standard, 2)))
print(paste("Total Costs for Long-term TMZ Therapy:", round(total_costs_long_term, 2)))

# Calculate incremental differences
incremental_costs <- total_costs_long_term - total_costs_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- total_qalys_long_term - total_qalys_standard

# Calculate ICERs
icer_qalms <- round(incremental_costs / incremental_qalms, 0)  # ICER per QALM
icer_qalys <- round(incremental_costs / incremental_qalys, 0)  # ICER per QALY

# Display results
print(paste("ICER per QALM:", icer_qalms))
print(paste("ICER per QALY:", icer_qalys))
