# Store input data

# Transition probabilities
transition_probs <- list(
  Standard_TMC = list(
    stable_to_progressive = 0.0956,
    stable_to_death = 0.0464,
    progressive_to_death = 0.0861
  ),
  Long_TMC = list(
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

# Cost data
costs <- list(
  Standard_TMC = data.frame(
    phase = c("month_1_3", "month_4_6", "subsequent", "progressive", "death"),
    cost = c(1944, 2453, 350, 2720, 0)
  ),
  Long_TMC = data.frame(
    phase = c("month_1_3", "month_4_6", "subsequent", "progressive", "death"),
    cost = c(1944, 2453, 2453, 2720, 0)
  )
)

# Model parameters
model_parameters <- list(
  cycle_length = 1,  # 1 month
  time_horizon = 60  # 60 months
)

# Data stored but not printed

# Define model parameters
time_horizon <- 60  # Number of cycles
initial_population <- 1000  # Cohort size

# Transition probabilities
transition_probs <- list(
  Standard_TMC = matrix(c(
    1 - 0.0956 - 0.0464, 0.0956, 0.0464,
    0, 1 - 0.0861, 0.0861,
    0, 0, 1
  ), nrow = 3, byrow = TRUE),
  
  Long_TMC = matrix(c(
    1 - 0.0894 - 0.0397, 0.0894, 0.0397,
    0, 1 - 0.0689, 0.0689,
    0, 0, 1
  ), nrow = 3, byrow = TRUE)
)

# Define states: Stable, Progressive, Death
state_names <- c("Stable", "Progressive", "Death")

# Initialize Markov traces for both treatments
markov_trace <- list(
  Standard_TMC = matrix(0, nrow = time_horizon, ncol = 3, dimnames = list(1:time_horizon, state_names)),
  Long_TMC = matrix(0, nrow = time_horizon, ncol = 3, dimnames = list(1:time_horizon, state_names))
)

# Initial state distribution: all patients start in "Stable"
markov_trace$Standard_TMC[1, ] <- c(initial_population, 0, 0)
markov_trace$Long_TMC[1, ] <- c(initial_population, 0, 0)

# Compute the Markov trace for both treatments over 60 cycles
for (t in 2:time_horizon) {
  markov_trace$Standard_TMC[t, ] <- markov_trace$Standard_TMC[t - 1, ] %*% transition_probs$Standard_TMC
  markov_trace$Long_TMC[t, ] <- markov_trace$Long_TMC[t - 1, ] %*% transition_probs$Long_TMC
}

# Output results
print("Markov trace for Standard TMZ therapy:")
print(markov_trace$Standard_TMC)

print("Markov trace for Long-term TMZ therapy:")
print(markov_trace$Long_TMC)

# Define health state utilities
health_utilities <- list(
  stable = 0.743,
  progressive = 0.731,
  decrement_per_month = 0.02
)

# Initialize QALMs matrix for both treatments
QALMs <- list(
  Standard_TMC = numeric(time_horizon),
  Long_TMC = numeric(time_horizon)
)

# Function to calculate progressive state utility with decrement applied
calculate_progressive_utility <- function(cycle) {
  if (cycle < 3) {
    return(health_utilities$progressive)
  } else if (cycle <= 25) {
    return(max(health_utilities$progressive - (cycle - 2) * health_utilities$decrement_per_month, 0))
  } else {
    return(max(health_utilities$progressive - (25 - 2) * health_utilities$decrement_per_month, 0))
  }
}

# Compute QALMs for each cycle
for (t in 1:time_horizon) {
  progressive_utility <- calculate_progressive_utility(t)
  
  QALMs$Standard_TMC[t] <- (markov_trace$Standard_TMC[t, "Stable"] / initial_population * health_utilities$stable) +
    (markov_trace$Standard_TMC[t, "Progressive"] / initial_population * progressive_utility)
  
  QALMs$Long_TMC[t] <- (markov_trace$Long_TMC[t, "Stable"] / initial_population * health_utilities$stable) +
    (markov_trace$Long_TMC[t, "Progressive"] / initial_population * progressive_utility)
}

# Calculate total QALMs
total_QALMs <- list(
  Standard_TMC = sum(QALMs$Standard_TMC),
  Long_TMC = sum(QALMs$Long_TMC)
)

# Print QALMs per cycle and total QALMs
print("QALMs per cycle for Standard TMZ therapy:")
print(QALMs$Standard_TMC)

print("QALMs per cycle for Long-term TMZ therapy:")
print(QALMs$Long_TMC)

print("Total QALMs for Standard TMZ therapy:")
print(total_QALMs$Standard_TMC)

print("Total QALMs for Long-term TMZ therapy:")
print(total_QALMs$Long_TMC)

# Convert total QALMs to total QALYs
total_QALYs <- list(
  Standard_TMC = total_QALMs$Standard_TMC / 12,
  Long_TMC = total_QALMs$Long_TMC / 12
)

# Print total QALYs for both treatments
print("Total QALYs for Standard TMZ therapy:")
print(total_QALYs$Standard_TMC)

print("Total QALYs for Long-term TMZ therapy:")
print(total_QALYs$Long_TMC)

# Initialize cost matrix for both treatments
cost_per_cycle <- list(
  Standard_TMC = numeric(time_horizon),
  Long_TMC = numeric(time_horizon)
)

# Assign costs based on cycle intervals
get_stable_cost <- function(cycle, treatment) {
  if (cycle <= 4) {
    return(costs[[treatment]]$cost[1])  # Months 1-3 → Cycles 1-4
  } else if (cycle <= 7) {
    return(costs[[treatment]]$cost[2])  # Months 4-6 → Cycles 5-7
  } else {
    return(costs[[treatment]]$cost[3])  # Subsequent months → Cycles 8-60
  }
}

# Compute cost per cycle
for (t in 1:time_horizon) {
  cost_per_cycle$Standard_TMC[t] <- (markov_trace$Standard_TMC[t, "Stable"] / initial_population * get_stable_cost(t, "Standard_TMC")) +
    (markov_trace$Standard_TMC[t, "Progressive"] / initial_population * costs$Standard_TMC$cost[4]) +
    (markov_trace$Standard_TMC[t, "Death"] / initial_population * costs$Standard_TMC$cost[5])
  
  cost_per_cycle$Long_TMC[t] <- (markov_trace$Long_TMC[t, "Stable"] / initial_population * get_stable_cost(t, "Long_TMC")) +
    (markov_trace$Long_TMC[t, "Progressive"] / initial_population * costs$Long_TMC$cost[4]) +
    (markov_trace$Long_TMC[t, "Death"] / initial_population * costs$Long_TMC$cost[5])
}

# Calculate total costs
total_costs <- list(
  Standard_TMC = sum(cost_per_cycle$Standard_TMC),
  Long_TMC = sum(cost_per_cycle$Long_TMC)
)

# Print cost per cycle and total costs
print("Cost per cycle for Standard TMZ therapy:")
print(cost_per_cycle$Standard_TMC)

print("Cost per cycle for Long-term TMZ therapy:")
print(cost_per_cycle$Long_TMC)

print("Total costs for Standard TMZ therapy:")
print(total_costs$Standard_TMC)

print("Total costs for Long-term TMZ therapy:")
print(total_costs$Long_TMC)

# Calculate ICER for QALMs
incremental_costs <- total_costs$Long_TMC - total_costs$Standard_TMC
incremental_QALMs <- total_QALMs$Long_TMC - total_QALMs$Standard_TMC

if (incremental_QALMs == 0) {
  ICER_QALMs <- Inf  # Avoid division by zero
} else {
  ICER_QALMs <- round(incremental_costs / incremental_QALMs, 0)
}

# Calculate ICER for QALYs
incremental_QALYs <- total_QALYs$Long_TMC - total_QALYs$Standard_TMC

if (incremental_QALYs == 0) {
  ICER_QALYs <- Inf  # Avoid division by zero
} else {
  ICER_QALYs <- round(incremental_costs / incremental_QALYs, 0)
}

# Print results
print("Incremental Cost-Effectiveness Ratio (ICER) for QALMs:")
print(ICER_QALMs)

print("Incremental Cost-Effectiveness Ratio (ICER) for QALYs:")
print(ICER_QALYs)
