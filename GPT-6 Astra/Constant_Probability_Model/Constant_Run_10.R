# Step 1: Store supplied inputs only. Sourcing this file prints no data.
# Transition probabilities correspond to the one-month model cycle.
tmz_inputs <- list(
  transition_probabilities = data.frame(
    treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
    stable_to_progressive = c(0.0956, 0.0894),
    stable_to_death = c(0.0464, 0.0397),
    progressive_to_death = c(0.0861, 0.0689),
    stringsAsFactors = FALSE
  ),
  utilities = list(
    stable = 0.743,
    progressive = 0.731,
    decrement_per_consecutive_month_of_progression = 0.02
  ),
  costs = data.frame(
    treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
    stable_months_1_to_3 = c(1944, 1944),
    stable_months_4_to_6 = c(2453, 2453),
    stable_subsequent_months = c(350, 2453),
    progressive = c(2720, 2720),
    death = c(0, 0),
    stringsAsFactors = FALSE
  ),
  design = list(
    cycle_length_months = 1,
    time_horizon_months = 60
  )
)

# Step 1: Store supplied inputs only. Sourcing this file prints no data.
# Transition probabilities correspond to the one-month model cycle.
tmz_inputs <- list(
  transition_probabilities = data.frame(
    treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
    stable_to_progressive = c(0.0956, 0.0894),
    stable_to_death = c(0.0464, 0.0397),
    progressive_to_death = c(0.0861, 0.0689),
    stringsAsFactors = FALSE
  ),
  utilities = list(
    stable = 0.743,
    progressive = 0.731,
    decrement_per_consecutive_month_of_progression = 0.02
  ),
  costs = data.frame(
    treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
    stable_months_1_to_3 = c(1944, 1944),
    stable_months_4_to_6 = c(2453, 2453),
    stable_subsequent_months = c(350, 2453),
    progressive = c(2720, 2720),
    death = c(0, 0),
    stringsAsFactors = FALSE
  ),
  design = list(
    cycle_length_months = 1,
    time_horizon_months = 60
  )
)

# Step 2: Calculate end-of-cycle occupancy for exactly 60 monthly cycles.
# Assumptions: all patients initially stable; no recovery from progression;
# death is absorbing. Initial occupancy is held separately, not as trace row 0.
states <- c("stable", "progressive", "death")
initial_state <- c(stable = 1, progressive = 0, death = 0)
n_cycles <- as.integer(tmz_inputs$design$time_horizon_months /
                         tmz_inputs$design$cycle_length_months)
stopifnot(n_cycles == 60L)

make_transition_matrix <- function(p) {
  # Rows are origin states; columns are destination states.
  transition_matrix <- matrix(c(
    1 - p$stable_to_progressive - p$stable_to_death,
    p$stable_to_progressive, p$stable_to_death,
    0, 1 - p$progressive_to_death, p$progressive_to_death,
    0, 0, 1
  ), nrow = 3L, byrow = TRUE, dimnames = list(states, states))
  stopifnot(all(transition_matrix >= 0), all(transition_matrix <= 1),
            all(abs(rowSums(transition_matrix) - 1) < 1e-12))
  transition_matrix
}

calculate_trace <- function(transition_matrix) {
  occupancy <- matrix(NA_real_, nrow = n_cycles, ncol = length(states),
                      dimnames = list(NULL, states))
  current_state <- initial_state
  for (cycle in seq_len(n_cycles)) {
    current_state <- as.numeric(current_state %*% transition_matrix)
    occupancy[cycle, ] <- current_state
  }
  trace <- data.frame(cycle = seq_len(n_cycles), occupancy,
                      row.names = NULL, check.names = FALSE)
  stopifnot(nrow(trace) == 60L, identical(trace$cycle, 1:60),
            all(occupancy >= 0), all(occupancy <= 1),
            all(abs(rowSums(occupancy) - 1) < 1e-12),
            all(diff(trace$death) >= -1e-12))
  trace
}

probabilities <- tmz_inputs$transition_probabilities
transition_matrices <- setNames(
  lapply(seq_len(nrow(probabilities)), function(i) {
    make_transition_matrix(probabilities[i, ])
  }), probabilities$treatment
)
markov_traces <- lapply(transition_matrices, calculate_trace)
trace_standard_tmz <- markov_traces[["Standard TMZ therapy"]]
trace_long_term_tmz <- markov_traces[["Long-term TMZ therapy"]]

# Each trace contains exactly 60 rows: cycle, stable, progressive, death.
# Values are cohort proportions. Inputs and results are stored without printing.

# Step 3: Calculate cycle-level and total QALMs from existing model objects.
# Run tmz_markov_trace.R first. No input data or traces are redefined here.
if (!exists("tmz_inputs") || !exists("markov_traces")) {
  stop("Run tmz_markov_trace.R before running this script.")
}

# Apply the explicitly requested calendar-cycle schedule to all progressive
# occupancy. This is not a patient-specific time-since-progression calculation.
# The 0.02 decrement is interpreted as a utility reduction per monthly step,
# consistent with the stored input. Utility x months gives QALMs.
# Cycles 1-2: no decrement; cycles 3-25: 23 decrements; cycles 26-60: hold.
# Death utility is assumed to be zero. No discounting or half-cycle correction.
calculate_qalms <- function(trace) {
  stopifnot(nrow(trace) == 60L, identical(as.integer(trace$cycle), 1:60))
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- tmz_inputs$utilities$progressive -
    tmz_inputs$utilities$decrement_per_consecutive_month_of_progression *
    decrement_count
  cycle_months <- tmz_inputs$design$cycle_length_months
  
  result <- data.frame(
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    stable_utility = tmz_inputs$utilities$stable,
    progressive_utility = progressive_utility,
    death_utility = 0,
    stable_qalms = trace$stable * tmz_inputs$utilities$stable * cycle_months,
    progressive_qalms = trace$progressive * progressive_utility * cycle_months,
    death_qalms = 0
  )
  result$total_qalms <- result$stable_qalms +
    result$progressive_qalms + result$death_qalms
  result$cumulative_qalms <- cumsum(result$total_qalms)
  
  stopifnot(
    all(decrement_count[1:2] == 0),
    all(decrement_count[3:25] == 1:23),
    all(decrement_count[26:60] == 23),
    all(result$progressive_utility[26:60] == result$progressive_utility[25])
  )
  result
}

qalm_results <- lapply(markov_traces, calculate_qalms)
qalms_standard_tmz <- qalm_results[["Standard TMZ therapy"]]
qalms_long_term_tmz <- qalm_results[["Long-term TMZ therapy"]]

total_qalms <- data.frame(
  treatment = names(qalm_results),
  total_qalms = vapply(qalm_results, function(x) sum(x$total_qalms), numeric(1)),
  row.names = NULL
)

# Display all 60 cycles for each treatment, followed by totals.
for (treatment in names(qalm_results)) {
  cat("\n", treatment, "\n", sep = "")
  print(qalm_results[[treatment]], row.names = FALSE, max = 10000)
}
cat("\nTotal QALMs per patient over 60 cycles\n")
print(total_qalms, row.names = FALSE)

total_qalys <- data.frame(
  treatment = total_qalms$treatment,
  total_qalys = total_qalms$total_qalms / 12
)
print(total_qalys, row.names = FALSE)

# Step 4: Calculate costs using the requested initial-state cycle convention.
# Run tmz_markov_trace.R first; reuse its inputs and end-of-month traces.
# No input data are redefined. Existing traces and QALY results are unchanged.
if (!exists("tmz_inputs") || !exists("markov_traces") ||
    !exists("initial_state")) {
  stop("Run tmz_markov_trace.R before running this script.")
}

calculate_costs <- function(treatment) {
  trace <- markov_traces[[treatment]]
  states <- c("stable", "progressive", "death")
  stopifnot(nrow(trace) == 60L, identical(as.integer(trace$cycle), 1:60))
  unit_costs <- tmz_inputs$costs[
    tmz_inputs$costs$treatment == treatment, , drop = FALSE
  ]
  stopifnot(nrow(unit_costs) == 1L)
  
  # Cost cycle 1 = initial occupancy (month 0).
  # Cost cycles 2-60 = end-of-month occupancy for months 1-59.
  # End-of-month 60 is deliberately excluded: exactly 60 cost cycles.
  occupancy <- rbind(
    as.numeric(initial_state[states]),
    as.matrix(trace[1:59, states])
  )
  colnames(occupancy) <- states
  cycle <- seq_len(60L)
  stable_unit_cost <- ifelse(
    cycle <= 4L, unit_costs$stable_months_1_to_3,
    ifelse(cycle <= 7L, unit_costs$stable_months_4_to_6,
           unit_costs$stable_subsequent_months)
  )
  
  result <- data.frame(
    cycle = cycle,
    month = cycle - 1L,
    stable = occupancy[, "stable"],
    progressive = occupancy[, "progressive"],
    death = occupancy[, "death"],
    stable_unit_cost = stable_unit_cost,
    progressive_unit_cost = unit_costs$progressive,
    death_unit_cost = unit_costs$death,
    row.names = NULL
  )
  # Monthly state costs weighted by the proportion in each state.
  # Cycle 1 incurs a full cycle cost, as requested.
  # No discounting or half-cycle correction is applied.
  result$stable_cost <- result$stable * result$stable_unit_cost
  result$progressive_cost <- result$progressive * result$progressive_unit_cost
  result$death_cost <- result$death * result$death_unit_cost
  result$total_cost <- result$stable_cost + result$progressive_cost +
    result$death_cost
  result$cumulative_cost <- cumsum(result$total_cost)
  
  stopifnot(
    nrow(result) == 60L,
    all(abs(rowSums(occupancy) - 1) < 1e-12),
    all(occupancy[1, ] == initial_state[states]),
    sum(cycle <= 4L) == 4L,
    sum(cycle >= 5L & cycle <= 7L) == 3L,
    sum(cycle >= 8L) == 53L
  )
  result
}

cost_results <- setNames(lapply(names(markov_traces), calculate_costs),
                         names(markov_traces))
costs_standard_tmz <- cost_results[["Standard TMZ therapy"]]
costs_long_term_tmz <- cost_results[["Long-term TMZ therapy"]]
total_costs <- data.frame(
  treatment = names(cost_results),
  total_cost = vapply(cost_results, function(x) sum(x$total_cost), numeric(1)),
  row.names = NULL
)

for (treatment in names(cost_results)) {
  cat("\n", treatment, "\n", sep = "")
  print(cost_results[[treatment]], row.names = FALSE, max = 10000)
}
cat("\nTotal costs per patient across cycles 1-60\n")
print(total_costs, row.names = FALSE)

# Step 5: ICERs for Long-term TMZ therapy versus Standard TMZ therapy.
# Run tmz_markov_trace.R, tmz_qalms.R and tmz_costs.R first.
# Reuse full-precision totals; do not redefine inputs or round intermediates.
# These ICERs retain the preceding scripts' timing conventions:
# costs use initial occupancy plus months 1-59; QALMs use months 1-60.
# Align timing before using these results as a consistent HTA base case.
if (!exists("total_costs") || !exists("total_qalms")) {
  stop("Run tmz_qalms.R and tmz_costs.R before running this script.")
}

get_total <- function(table, treatment, column) {
  value <- table[table$treatment == treatment, column]
  if (length(value) != 1L || !is.finite(value)) {
    stop("Expected one finite total for: ", treatment)
  }
  value
}

incremental_cost <-
  get_total(total_costs, "Long-term TMZ therapy", "total_cost") -
  get_total(total_costs, "Standard TMZ therapy", "total_cost")
incremental_qalms <-
  get_total(total_qalms, "Long-term TMZ therapy", "total_qalms") -
  get_total(total_qalms, "Standard TMZ therapy", "total_qalms")
incremental_qalys <- incremental_qalms / 12

if (incremental_qalms == 0) {
  stop("ICER is undefined because incremental health benefit is zero.")
}

icer_results <- data.frame(
  outcome = c("QALM", "QALY"),
  incremental_cost = incremental_cost,
  incremental_effect = c(incremental_qalms, incremental_qalys),
  icer = incremental_cost / c(incremental_qalms, incremental_qalys)
)

# Keep numeric ICERs at full precision; display with no decimal places.
icer_display <- data.frame(
  measure = c("Cost per additional QALM", "Cost per additional QALY"),
  ICER = formatC(icer_results$icer, format = "f", digits = 0, big.mark = ",")
)
cat("Long-term TMZ therapy versus Standard TMZ therapy\n")
print(icer_display, row.names = FALSE, right = FALSE)