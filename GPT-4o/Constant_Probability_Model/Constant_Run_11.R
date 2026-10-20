# Store input data for the Markov model

# Transition probabilities for Standard TMZ therapy
tp_standard <- list(
  stable_to_progressive = 0.0956,
  stable_to_death = 0.0464,
  progressive_to_death = 0.0861
)

# Transition probabilities for Long-term TMZ therapy
tp_longterm <- list(
  stable_to_progressive = 0.0894,
  stable_to_death = 0.0397,
  progressive_to_death = 0.0689
)

# Health utilities
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Costs for Standard TMZ therapy
costs_standard <- list(
  stable_1_3 = 1944,
  stable_4_6 = 2453,
  stable_subsequent = 350,
  progressive = 2720,
  death = 0
)

# Costs for Long-term TMZ therapy
costs_longterm <- list(
  stable_1_3 = 1944,
  stable_4_6 = 2453,
  stable_subsequent = 2453,
  progressive = 2720,
  death = 0
)

# Model parameters
model_params <- list(
  cycle_length = 1,  # in months
  time_horizon = 60  # total cycles (months)
)

# Define the number of cycles
num_cycles <- 60  

# Define the Markov states
states <- c("Stable", "Progressive", "Death")

# Transition probability matrices for Standard TMZ therapy
trans_matrix_standard <- matrix(c(
  1 - (0.0956 + 0.0464), 0.0956, 0.0464,  # Stable state
  0, 1 - 0.0861, 0.0861,                  # Progressive state
  0, 0, 1                                 # Death state
), nrow = 3, byrow = TRUE, dimnames = list(states, states))

# Transition probability matrices for Long-term TMZ therapy
trans_matrix_longterm <- matrix(c(
  1 - (0.0894 + 0.0397), 0.0894, 0.0397,  # Stable state
  0, 1 - 0.0689, 0.0689,                  # Progressive state
  0, 0, 1                                 # Death state
), nrow = 3, byrow = TRUE, dimnames = list(states, states))

# Initialize Markov traces
trace_standard <- matrix(0, nrow = num_cycles, ncol = 3, dimnames = list(1:num_cycles, states))
trace_longterm <- matrix(0, nrow = num_cycles, ncol = 3, dimnames = list(1:num_cycles, states))

# Initial distribution (100% in Stable state)
trace_standard[1, ] <- c(1, 0, 0)
trace_longterm[1, ] <- c(1, 0, 0)

# Compute the Markov trace over time
for (t in 2:num_cycles) {
  trace_standard[t, ] <- trace_standard[t - 1, ] %*% trans_matrix_standard
  trace_longterm[t, ] <- trace_longterm[t - 1, ] %*% trans_matrix_longterm
}

# Store the results in data frames for better visualization
trace_standard_df <- as.data.frame(trace_standard)
trace_longterm_df <- as.data.frame(trace_longterm)
trace_standard_df$Cycle <- 1:num_cycles
trace_longterm_df$Cycle <- 1:num_cycles

# Display the traces
print(head(trace_standard_df, 10))  # Show first 10 cycles for Standard TMZ
print(head(trace_longterm_df, 10))  # Show first 10 cycles for Long-term TMZ

# If needed, write to CSV for further analysis
# write.csv(trace_standard_df, "Markov_Trace_Standard_Therapy.csv", row.names = FALSE)
# write.csv(trace_longterm_df, "Markov_Trace_LongTerm_Therapy.csv", row.names = FALSE)

# Define health state utilities
utility_stable <- 0.743
utility_progressive <- 0.731
decrement_per_month <- 0.02
decrement_start <- 3  # Start decrement from cycle 3
decrement_end <- 25   # Apply decrement up to cycle 25
max_decrement_cycles <- decrement_end - decrement_start + 1

# Function to compute utility for the progressive state based on cycle number
compute_progressive_utility <- function(cycle) {
  if (cycle < decrement_start) {
    return(utility_progressive)
  } else if (cycle >= decrement_start & cycle <= decrement_end) {
    decremented_utility <- utility_progressive - ((cycle - decrement_start + 1) * decrement_per_month)
    return(max(decremented_utility, 0))  # Ensure utility doesn't go negative
  } else {
    return(utility_progressive - (max_decrement_cycles * decrement_per_month))  # Fixed at last decrement value
  }
}

# Initialize QALMs data frames
qalms_standard <- data.frame(Cycle = 1:num_cycles, QALMs = rep(0, num_cycles))
qalms_longterm <- data.frame(Cycle = 1:num_cycles, QALMs = rep(0, num_cycles))

# Compute QALMs for each cycle
for (t in 1:num_cycles) {
  # Get utility values for the progressive state based on cycle
  progressive_utility <- compute_progressive_utility(t)
  
  # Compute QALMs for Standard TMZ therapy
  qalms_standard$QALMs[t] <- (trace_standard[t, "Stable"] * utility_stable) +
    (trace_standard[t, "Progressive"] * progressive_utility)
  
  # Compute QALMs for Long-term TMZ therapy
  qalms_longterm$QALMs[t] <- (trace_longterm[t, "Stable"] * utility_stable) +
    (trace_longterm[t, "Progressive"] * progressive_utility)
}

# Calculate total QALMs
total_qalms_standard <- sum(qalms_standard$QALMs)
total_qalms_longterm <- sum(qalms_longterm$QALMs)

# Display the QALMs results for each cycle
print(head(qalms_standard, 10))  # Show first 10 cycles for Standard TMZ
print(head(qalms_longterm, 10))  # Show first 10 cycles for Long-term TMZ

# Display the total QALMs
cat("Total QALMs for Standard TMZ therapy:", total_qalms_standard, "\n")
cat("Total QALMs for Long-term TMZ therapy:", total_qalms_longterm, "\n")

# If needed, write results to CSV
# write.csv(qalms_standard, "QALMs_Standard_Therapy.csv", row.names = FALSE)
# write.csv(qalms_longterm, "QALMs_LongTerm_Therapy.csv", row.names = FALSE)

# Convert total QALMs to total QALYs by dividing by 12
total_qalys_standard <- total_qalms_standard / 12
total_qalys_longterm <- total_qalms_longterm / 12

# Display the total QALYs for both treatments
cat("Total QALYs for Standard TMZ therapy:", total_qalys_standard, "\n")
cat("Total QALYs for Long-term TMZ therapy:", total_qalys_longterm, "\n")

# Initialize cost data frames
costs_standard_df <- data.frame(Cycle = 1:num_cycles, Costs = rep(0, num_cycles))
costs_longterm_df <- data.frame(Cycle = 1:num_cycles, Costs = rep(0, num_cycles))

# Function to determine cost per cycle based on the cycle number
compute_cost <- function(cycle, trace, costs) {
  # Determine stable state cost
  if (cycle <= 4) {  # Cycles 1-4 (Months 1-3)
    stable_cost <- costs$stable_1_3
  } else if (cycle >= 5 & cycle <= 7) {  # Cycles 5-7 (Months 4-6)
    stable_cost <- costs$stable_4_6
  } else {  # Cycles 8-60 (Months 7+)
    stable_cost <- costs$stable_subsequent
  }
  
  # Compute total cost for this cycle
  total_cost <- (trace[cycle, "Stable"] * stable_cost) +
    (trace[cycle, "Progressive"] * costs$progressive) +
    (trace[cycle, "Death"] * costs$death)
  
  return(total_cost)
}

# Compute costs per cycle for both treatments
for (t in 1:num_cycles) {
  costs_standard_df$Costs[t] <- compute_cost(t, trace_standard, costs_standard)
  costs_longterm_df$Costs[t] <- compute_cost(t, trace_longterm, costs_longterm)
}

# Calculate total costs by summing over all cycles
total_cost_standard <- sum(costs_standard_df$Costs)
total_cost_longterm <- sum(costs_longterm_df$Costs)

# Display costs for each cycle (first 10 cycles)
print(head(costs_standard_df, 10))  # Standard TMZ
print(head(costs_longterm_df, 10))  # Long-term TMZ

# Display total costs
cat("Total cost for Standard TMZ therapy:", total_cost_standard, "\n")
cat("Total cost for Long-term TMZ therapy:", total_cost_longterm, "\n")

# Optionally, export results to CSV
# write.csv(costs_standard_df, "Costs_Standard_Therapy.csv", row.names = FALSE)
# write.csv(costs_longterm_df, "Costs_LongTerm_Therapy.csv", row.names = FALSE)

# Calculate incremental costs and effects
incremental_cost <- total_cost_longterm - total_cost_standard
incremental_qalms <- total_qalms_longterm - total_qalms_standard
incremental_qalys <- total_qalys_longterm - total_qalys_standard

# Calculate ICERs (rounded to nearest whole number)
icer_qalms <- round(incremental_cost / incremental_qalms, 0)
icer_qalys <- round(incremental_cost / incremental_qalys, 0)

# Display ICERs
cat("ICER for QALMs:", icer_qalms, "cost per QALM\n")
cat("ICER for QALYs:", icer_qalys, "cost per QALY\n")
