# Store input parameters

# Transition probabilities
trans_probs <- list(
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

# Health utility values
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Costs
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

# Model parameters
cycle_length <- 1  # in months
time_horizon <- 60 # in months
num_cycles <- time_horizon / cycle_length

# Markov model for Standard TMZ and Long-term TMZ therapies

# Define model parameters

# Transition probabilities
trans_probs <- list(
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

# Model settings
time_horizon <- 60  # 60 cycles (each cycle = 1 month)

# Initial state distribution (100% of patients start in stable state)
initial_state <- c(1, 0, 0)  # (Stable, Progressive, Death)

# Function to calculate Markov trace
calculate_markov_trace <- function(trans_probs, time_horizon) {
  # Create Markov trace matrix (rows = cycles, cols = states)
  trace <- matrix(0, nrow = time_horizon, ncol = 3)
  colnames(trace) <- c("Stable", "Progressive", "Death")
  
  # Initialize first row with initial state
  trace[1, ] <- initial_state
  
  # Iterate through cycles
  for (t in 2:time_horizon) {
    stable <- trace[t-1, 1] * (1 - trans_probs$stable_to_progressive - trans_probs$stable_to_death)
    progressive <- trace[t-1, 1] * trans_probs$stable_to_progressive + trace[t-1, 2] * (1 - trans_probs$progressive_to_death)
    death <- trace[t-1, 1] * trans_probs$stable_to_death + trace[t-1, 2] * trans_probs$progressive_to_death + trace[t-1, 3]
    
    trace[t, ] <- c(stable, progressive, death)
  }
  
  return(trace)
}

# Compute Markov traces for both therapies
trace_standard_TMZ <- calculate_markov_trace(trans_probs$standard_TMZ, time_horizon)
trace_long_term_TMZ <- calculate_markov_trace(trans_probs$long_term_TMZ, time_horizon)

# Convert to data frames for output
trace_standard_TMZ_df <- data.frame(Cycle = 1:time_horizon, trace_standard_TMZ)
trace_long_term_TMZ_df <- data.frame(Cycle = 1:time_horizon, trace_long_term_TMZ)

# Save the results (optional)
write.csv(trace_standard_TMZ_df, "markov_trace_standard_TMZ.csv", row.names = FALSE)
write.csv(trace_long_term_TMZ_df, "markov_trace_long_term_TMZ.csv", row.names = FALSE)

# Output Markov traces
print(head(trace_standard_TMZ_df))  # Print first few rows of Standard TMZ trace
print(head(trace_long_term_TMZ_df)) # Print first few rows of Long-term TMZ trace

# Define health utility values
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Function to calculate QALMs for each cycle
calculate_QALMs <- function(markov_trace, health_utilities, time_horizon) {
  QALMs <- numeric(time_horizon)  # Initialize QALMs vector
  
  for (t in 1:time_horizon) {
    stable_QALM <- markov_trace[t, "Stable"] * health_utilities$stable
    
    # Apply decrement for progressive state
    if (t < 3) {
      progressive_QALM <- markov_trace[t, "Progressive"] * health_utilities$progressive
    } else if (t >= 3 && t <= 25) {
      decremented_utility <- max(health_utilities$progressive - (t - 2) * health_utilities$decrement_per_month, 0)
      progressive_QALM <- markov_trace[t, "Progressive"] * decremented_utility
    } else {
      fixed_utility <- health_utilities$progressive - (25 - 2) * health_utilities$decrement_per_month
      progressive_QALM <- markov_trace[t, "Progressive"] * fixed_utility
    }
    
    death_QALM <- markov_trace[t, "Death"] * 0  # Death state has 0 QALMs
    
    # Sum up QALMs for this cycle
    QALMs[t] <- stable_QALM + progressive_QALM + death_QALM
  }
  
  return(QALMs)
}

# Calculate QALMs for each cycle
QALMs_standard_TMZ <- calculate_QALMs(trace_standard_TMZ_df, health_utilities, time_horizon)
QALMs_long_term_TMZ <- calculate_QALMs(trace_long_term_TMZ_df, health_utilities, time_horizon)

# Calculate total QALMs
total_QALMs_standard_TMZ <- sum(QALMs_standard_TMZ)
total_QALMs_long_term_TMZ <- sum(QALMs_long_term_TMZ)

# Create data frame for results
QALMs_results <- data.frame(
  Cycle = 1:time_horizon,
  Standard_TMZ_QALMs = QALMs_standard_TMZ,
  Long_Term_TMZ_QALMs = QALMs_long_term_TMZ
)

# Save the results (optional)
write.csv(QALMs_results, "QALMs_results.csv", row.names = FALSE)

# Output first few rows and total QALMs
print(head(QALMs_results))  # Print first few cycles
cat("Total QALMs for Standard TMZ Therapy:", total_QALMs_standard_TMZ, "\n")
cat("Total QALMs for Long-term TMZ Therapy:", total_QALMs_long_term_TMZ, "\n")

# Calculate total QALYs by summing QALMs and converting to years
total_QALYs_standard_TMZ <- sum(QALMs_standard_TMZ) / 12
total_QALYs_long_term_TMZ <- sum(QALMs_long_term_TMZ) / 12

# Print the results
cat("Total QALYs for Standard TMZ Therapy:", total_QALYs_standard_TMZ, "\n")
cat("Total QALYs for Long-term TMZ Therapy:", total_QALYs_long_term_TMZ, "\n")

# Function to calculate costs for each cycle
calculate_costs <- function(markov_trace, costs, time_horizon) {
  total_costs <- numeric(time_horizon)  # Initialize cost vector
  
  for (t in 1:time_horizon) {
    # Determine cost category based on cycle number
    if (t <= 4) {
      stable_cost <- costs$stable_1_3  # Cost for months 1-3 applied to cycles 1-4
    } else if (t >= 5 && t <= 7) {
      stable_cost <- costs$stable_4_6  # Cost for months 4-6 applied to cycles 5-7
    } else {
      stable_cost <- costs$stable_subsequent  # Cost for subsequent months applied from cycle 8 onwards
    }
    
    # Calculate total cost for this cycle
    cycle_cost <- (markov_trace[t, "Stable"] * stable_cost) +
      (markov_trace[t, "Progressive"] * costs$progressive) +
      (markov_trace[t, "Death"] * costs$death)
    
    total_costs[t] <- cycle_cost
  }
  
  return(total_costs)
}

# Calculate costs for each cycle
costs_standard_TMZ <- calculate_costs(trace_standard_TMZ_df, costs$standard_TMZ, time_horizon)
costs_long_term_TMZ <- calculate_costs(trace_long_term_TMZ_df, costs$long_term_TMZ, time_horizon)

# Calculate total costs
total_cost_standard_TMZ <- sum(costs_standard_TMZ)
total_cost_long_term_TMZ <- sum(costs_long_term_TMZ)

# Create data frame for results
cost_results <- data.frame(
  Cycle = 1:time_horizon,
  Standard_TMZ_Costs = costs_standard_TMZ,
  Long_Term_TMZ_Costs = costs_long_term_TMZ
)

# Save the results (optional)
write.csv(cost_results, "cost_results.csv", row.names = FALSE)

# Output first few rows and total costs
print(head(cost_results))  # Print first few cycles
cat("Total Costs for Standard TMZ Therapy:", total_cost_standard_TMZ, "\n")
cat("Total Costs for Long-term TMZ Therapy:", total_cost_long_term_TMZ, "\n")

# Calculate incremental costs and effects
incremental_cost <- total_cost_long_term_TMZ - total_cost_standard_TMZ
incremental_QALMs <- sum(QALMs_long_term_TMZ) - sum(QALMs_standard_TMZ)
incremental_QALYs <- total_QALYs_long_term_TMZ - total_QALYs_standard_TMZ

# Calculate ICERs (rounded to whole numbers)
ICER_QALMs <- round(incremental_cost / incremental_QALMs)
ICER_QALYs <- round(incremental_cost / incremental_QALYs)

# Print results
cat("Incremental Cost for Long-term TMZ vs Standard TMZ:", incremental_cost, "\n")
cat("Incremental QALMs:", incremental_QALMs, "\n")
cat("Incremental QALYs:", incremental_QALYs, "\n")
cat("ICER (Cost per QALM):", ICER_QALMs, "\n")
cat("ICER (Cost per QALY):", ICER_QALYs, "\n")
