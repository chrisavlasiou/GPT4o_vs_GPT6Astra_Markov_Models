# Store transition probabilities
transition_probs <- list(
  Standard_TMZ = list(
    stable_to_progressive = 0.0956,
    stable_to_death = 0.0464,
    progressive_to_death = 0.0861
  ),
  Long_term_TMZ = list(
    stable_to_progressive = 0.0894,
    stable_to_death = 0.0397,
    progressive_to_death = 0.0689
  )
)

# Store health utilities
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Store costs
costs <- list(
  Standard_TMZ = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 350,
    progressive = 2720,
    death = 0
  ),
  Long_term_TMZ = list(
    stable_1_3 = 1944,
    stable_4_6 = 2453,
    stable_subsequent = 2453,
    progressive = 2720,
    death = 0
  )
)

# Store model time parameters
time_params <- list(
  cycle_length = 1,   # 1 month
  time_horizon = 60   # 60 months
)

# Load necessary library
library(dplyr)

# Define parameters
n_cycles <- 60  # Number of cycles (months)

# Define transition probabilities
transition_probs <- list(
  Standard_TMZ = c(0.0956, 0.0464, 0.0861),  # (Stable -> Progressive, Stable -> Death, Progressive -> Death)
  Long_term_TMZ = c(0.0894, 0.0397, 0.0689) # (Stable -> Progressive, Stable -> Death, Progressive -> Death)
)

# Initialize Markov traces
markov_trace <- function(prob) {
  trace <- matrix(0, nrow = n_cycles, ncol = 3)
  colnames(trace) <- c("Stable", "Progressive", "Death")
  
  # Initial state distribution: 100% in Stable
  trace[1, ] <- c(1, 0, 0)
  
  # Iterate through cycles
  for (t in 2:n_cycles) {
    prev <- trace[t - 1, ]
    
    trace[t, 1] <- prev[1] * (1 - prob[1] - prob[2])  # Stay in Stable
    trace[t, 2] <- prev[1] * prob[1] + prev[2] * (1 - prob[3])  # Enter or remain in Progressive
    trace[t, 3] <- prev[1] * prob[2] + prev[2] * prob[3] + prev[3]  # Enter Death (absorbing)
  }
  
  return(trace)
}

# Compute traces for both treatments
trace_standard <- markov_trace(transition_probs$Standard_TMZ)
trace_long_term <- markov_trace(transition_probs$Long_term_TMZ)

# Convert to data frames for easier handling
trace_standard_df <- as.data.frame(trace_standard) %>%
  mutate(Cycle = 1:n_cycles)

trace_long_term_df <- as.data.frame(trace_long_term) %>%
  mutate(Cycle = 1:n_cycles)

# Save results
write.csv(trace_standard_df, "markov_trace_standard_tmz.csv", row.names = FALSE)
write.csv(trace_long_term_df, "markov_trace_long_term_tmz.csv", row.names = FALSE)

# Print final cycle results for verification
print(tail(trace_standard_df, 1))
print(tail(trace_long_term_df, 1))

# Define health state utilities
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Function to calculate QALMs per cycle
calculate_qalms <- function(markov_trace) {
  n_cycles <- nrow(markov_trace)
  qalms <- numeric(n_cycles)
  
  # Initialize progressive state utility adjustments
  progressive_utilities <- rep(health_utilities$progressive, n_cycles)
  
  # Apply decrement for cycles 3 to 25 (i.e., second consecutive month of progression onward)
  for (t in 3:25) {
    progressive_utilities[t] <- health_utilities$progressive - (t - 2) * health_utilities$decrement_per_month
  }
  
  # Fix the progressive utility from cycle 26 onward to the cycle 25 value
  progressive_utilities[26:n_cycles] <- progressive_utilities[25]
  
  # Compute QALMs for each cycle
  for (t in 1:n_cycles) {
    qalms[t] <- (markov_trace[t, "Stable"] * health_utilities$stable) +
      (markov_trace[t, "Progressive"] * progressive_utilities[t])
  }
  
  return(qalms)
}

# Compute QALMs for both treatments
qalms_standard <- calculate_qalms(trace_standard_df)
qalms_long_term <- calculate_qalms(trace_long_term_df)

# Convert results to data frames
qalms_standard_df <- data.frame(Cycle = 1:n_cycles, QALMs = qalms_standard)
qalms_long_term_df <- data.frame(Cycle = 1:n_cycles, QALMs = qalms_long_term)

# Calculate total QALMs
total_qalms_standard <- sum(qalms_standard)
total_qalms_long_term <- sum(qalms_long_term)

# Save results
write.csv(qalms_standard_df, "qalms_standard_tmz.csv", row.names = FALSE)
write.csv(qalms_long_term_df, "qalms_long_term_tmz.csv", row.names = FALSE)

# Print total QALMs for both treatments
cat("Total QALMs for Standard TMZ Therapy:", total_qalms_standard, "\n")
cat("Total QALMs for Long-term TMZ Therapy:", total_qalms_long_term, "\n")

# Print QALMs per cycle for verification
print(head(qalms_standard_df, 10))  # First 10 cycles
print(head(qalms_long_term_df, 10))  # First 10 cycles

# Convert total QALMs to total QALYs
total_qalys_standard <- total_qalms_standard / 12
total_qalys_long_term <- total_qalms_long_term / 12

# Print results
cat("Total QALYs for Standard TMZ Therapy:", total_qalys_standard, "\n")
cat("Total QALYs for Long-term TMZ Therapy:", total_qalys_long_term, "\n")

# Function to calculate costs per cycle
calculate_costs <- function(markov_trace, cost_structure) {
  n_cycles <- nrow(markov_trace)
  costs_per_cycle <- numeric(n_cycles)
  
  # Define cost assignments based on cycle structure
  for (t in 1:n_cycles) {
    if (t <= 4) {
      stable_cost <- cost_structure$stable_1_3  # Cycles 1-4
    } else if (t <= 7) {
      stable_cost <- cost_structure$stable_4_6  # Cycles 5-7
    } else {
      stable_cost <- cost_structure$stable_subsequent  # Cycles 8-60
    }
    
    # Compute costs per cycle
    costs_per_cycle[t] <- (markov_trace[t, "Stable"] * stable_cost) +
      (markov_trace[t, "Progressive"] * cost_structure$progressive) +
      (markov_trace[t, "Death"] * cost_structure$death)
  }
  
  return(costs_per_cycle)
}

# Compute costs per cycle for both treatments
costs_standard <- calculate_costs(trace_standard_df, costs$Standard_TMZ)
costs_long_term <- calculate_costs(trace_long_term_df, costs$Long_term_TMZ)

# Convert results to data frames
costs_standard_df <- data.frame(Cycle = 1:n_cycles, Costs = costs_standard)
costs_long_term_df <- data.frame(Cycle = 1:n_cycles, Costs = costs_long_term)

# Calculate total costs
total_costs_standard <- sum(costs_standard)
total_costs_long_term <- sum(costs_long_term)

# Save results
write.csv(costs_standard_df, "costs_standard_tmz.csv", row.names = FALSE)
write.csv(costs_long_term_df, "costs_long_term_tmz.csv", row.names = FALSE)

# Print total costs for both treatments
cat("Total Costs for Standard TMZ Therapy:", total_costs_standard, "\n")
cat("Total Costs for Long-term TMZ Therapy:", total_costs_long_term, "\n")

# Print costs per cycle for verification
print(head(costs_standard_df, 10))  # First 10 cycles
print(head(costs_long_term_df, 10))  # First 10 cycles

# Calculate incremental values
incremental_costs <- total_costs_long_term - total_costs_standard
incremental_qalms <- total_qalms_long_term - total_qalms_standard
incremental_qalys <- total_qalys_long_term - total_qalys_standard

# Calculate ICERs
icer_qalms <- round(incremental_costs / incremental_qalms, 0)  # ICER per QALM
icer_qalys <- round(incremental_costs / incremental_qalys, 0)  # ICER per QALY

# Print results without decimals
cat("Incremental Cost-Effectiveness Ratio (ICER) per QALM:", icer_qalms, "\n")
cat("Incremental Cost-Effectiveness Ratio (ICER) per QALY:", icer_qalys, "\n")
