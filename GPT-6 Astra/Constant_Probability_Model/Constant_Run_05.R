# Step 1: Store supplied inputs. Sourcing this file produces no output.
# Currency and further modelling assumptions have not yet been specified.

tmz_inputs <- list(
  states = c("stable", "progressive", "death"),
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

# Step 1: Store supplied inputs. Sourcing this file produces no output.
# Currency and further modelling assumptions have not yet been specified.

tmz_inputs <- list(
  states = c("stable", "progressive", "death"),
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

# Step 2: Calculate end-of-cycle cohort proportions over exactly 60 months.
# Assumptions: all patients start stable before cycle 1; transition
# probabilities apply per month; no recovery from progression; death absorbing.
# The initial distribution is not an output row. Cycle 1 follows one transition,
# and cycle 60 follows 60 transitions. No cycle 0 or cycle 61 is included.

n_cycles <- as.integer(
  tmz_inputs$design$time_horizon_months / tmz_inputs$design$cycle_length_months
)
stopifnot(n_cycles == 60L)
initial_distribution <- c(stable = 1, progressive = 0, death = 0)

make_transition_matrix <- function(stable_to_progressive, stable_to_death,
                                   progressive_to_death) {
  # Rows = origin state; columns = destination state.
  transition_matrix <- matrix(
    c(1 - stable_to_progressive - stable_to_death,
      stable_to_progressive, stable_to_death,
      0, 1 - progressive_to_death, progressive_to_death,
      0, 0, 1),
    nrow = 3L, byrow = TRUE,
    dimnames = list(from = tmz_inputs$states, to = tmz_inputs$states)
  )
  stopifnot(
    all(is.finite(transition_matrix)),
    all(transition_matrix >= 0 & transition_matrix <= 1),
    all(abs(rowSums(transition_matrix) - 1) < 1e-12)
  )
  transition_matrix
}

calculate_trace <- function(transition_matrix) {
  occupancy <- matrix(
    NA_real_, nrow = n_cycles, ncol = length(tmz_inputs$states),
    dimnames = list(NULL, tmz_inputs$states)
  )
  current_distribution <- initial_distribution
  for (cycle in seq_len(n_cycles)) {
    current_distribution <- drop(current_distribution %*% transition_matrix)
    occupancy[cycle, ] <- current_distribution
  }
  trace <- data.frame(cycle = seq_len(n_cycles), occupancy, row.names = NULL)
  stopifnot(
    nrow(trace) == 60L,
    identical(trace$cycle, 1:60),
    all(is.finite(occupancy)),
    all(occupancy >= -1e-12 & occupancy <= 1 + 1e-12),
    all(abs(rowSums(occupancy) - 1) < 1e-12),
    all(diff(trace$death) >= -1e-12)
  )
  trace
}

transition_matrices <- setNames(
  lapply(seq_len(nrow(tmz_inputs$transition_probabilities)), function(i) {
    probabilities <- tmz_inputs$transition_probabilities[i, ]
    make_transition_matrix(
      probabilities$stable_to_progressive,
      probabilities$stable_to_death,
      probabilities$progressive_to_death
    )
  }),
  tmz_inputs$transition_probabilities$treatment
)
markov_traces <- lapply(transition_matrices, calculate_trace)
trace_standard_tmz <- markov_traces[["Standard TMZ therapy"]]
trace_long_term_tmz <- markov_traces[["Long-term TMZ therapy"]]

# Both traces remain in memory without automatically printing inputs or results.
# To view the results interactively, run:
# View(trace_standard_tmz)
# View(trace_long_term_tmz)

# Step 3: QALMs using the inputs and Markov traces already in memory.
# Run tmz_markov_trace.R first. This script does not redefine input data.
# Outputs: qalm_standard_tmz, qalm_long_term_tmz, qalm_totals.
# Uses end-of-cycle occupancy, with no discounting or half-cycle correction.
# Death utility is assumed to be zero.
# The requested 0.02 is treated as a decrement in utility, not an absolute
# loss of 0.02 QALYs. Utility * cycle length in months yields QALMs.
# The explicit cycle 3:25 schedule applies to everyone currently progressive.
# This is a calendar-cycle schedule, not individual time since progression;
# tracking individual progression duration would require additional tracking.

if (!exists("tmz_inputs") || !exists("trace_standard_tmz") ||
    !exists("trace_long_term_tmz")) {
  stop("Run tmz_markov_trace.R first to create the existing inputs and traces.")
}

calculate_qalms <- function(trace) {
  stopifnot(
    nrow(trace) == 60L,
    all(trace$cycle == seq_len(60L)),
    tmz_inputs$design$cycle_length_months == 1
  )
  # Cycles 1-2: zero decrements; cycles 3-25: 1-23 decrements;
  # cycles 26-60: hold at 23 decrements, preserving the cycle-25 utility.
  decrements <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- tmz_inputs$utilities$progressive -
    tmz_inputs$utilities$decrement_per_consecutive_month_of_progression *
    decrements
  cycle_months <- tmz_inputs$design$cycle_length_months
  
  result <- data.frame(
    cycle = trace$cycle,
    stable = trace$stable,
    progressive = trace$progressive,
    death = trace$death,
    progressive_utility = progressive_utility,
    qalms_stable = trace$stable * tmz_inputs$utilities$stable * cycle_months,
    qalms_progressive = trace$progressive * progressive_utility * cycle_months,
    qalms_death = trace$death * 0 * cycle_months
  )
  result$qalms_cycle <- result$qalms_stable + result$qalms_progressive +
    result$qalms_death
  result$qalms_cumulative <- cumsum(result$qalms_cycle)
  
  # Check decrement boundaries and the unchanged utility after cycle 25.
  stopifnot(
    sum(diff(decrements) == 1L) == 23L,
    decrements[2L] == 0L, decrements[3L] == 1L,
    decrements[25L] == 23L,
    all(progressive_utility[26:60] == progressive_utility[25L]),
    all(is.finite(result$qalms_cycle))
  )
  result
}

qalm_standard_tmz <- calculate_qalms(trace_standard_tmz)
qalm_long_term_tmz <- calculate_qalms(trace_long_term_tmz)
qalm_totals <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_qalms = c(sum(qalm_standard_tmz$qalms_cycle),
                  sum(qalm_long_term_tmz$qalms_cycle))
)

# Print all 60 cycles for each treatment, then the totals.
cat("\nStandard TMZ therapy: QALMs by cycle\n")
print(qalm_standard_tmz, row.names = FALSE, max = 10000L)
cat("\nLong-term TMZ therapy: QALMs by cycle\n")
print(qalm_long_term_tmz, row.names = FALSE, max = 10000L)
cat("\nTotal QALMs over cycles 1-60\n")
print(qalm_totals, row.names = FALSE)

qalm_totals$total_qalys <- qalm_totals$total_qalms / 12
print(qalm_totals, row.names = FALSE)

# Step 4: Costs for exactly 60 cycles, using existing inputs and matrices.
# Run tmz_markov_trace.R first. No input data are redefined here.
# Cycle 1 = initial distribution; cycle 2 = month 1; cycle 60 = month 59.
# Charge cycle 1 as requested, giving 60 cost-bearing rows and 59 transitions.
# Costs are undiscounted per initial patient, without half-cycle correction.
# Previous QALM/QALY outputs used a different (end-of-cycle) indexing convention.

if (!exists("tmz_inputs") || !exists("transition_matrices") ||
    !exists("initial_distribution")) {
  stop("Run tmz_markov_trace.R first to create inputs and transition matrices.")
}

calculate_cycle_costs <- function(treatment) {
  states <- tmz_inputs$states
  transition_matrix <- transition_matrices[[treatment]][states, states]
  unit_costs <- tmz_inputs$costs[
    tmz_inputs$costs$treatment == treatment, , drop = FALSE
  ]
  stopifnot(nrow(unit_costs) == 1L,
            tmz_inputs$design$cycle_length_months == 1)
  
  # Build a separate cost trace with the requested initial-state row.
  occupancy <- matrix(NA_real_, nrow = 60L, ncol = length(states),
                      dimnames = list(NULL, states))
  occupancy[1L, ] <- initial_distribution[states]
  for (cycle in 2:60) {
    occupancy[cycle, ] <- occupancy[cycle - 1L, ] %*% transition_matrix
  }
  cycles <- seq_len(60L)
  stable_unit_cost <- ifelse(
    cycles <= 4L, unit_costs$stable_months_1_to_3,
    ifelse(cycles <= 7L, unit_costs$stable_months_4_to_6,
           unit_costs$stable_subsequent_months)
  )
  
  result <- data.frame(
    cycle = cycles,
    month = cycles - 1L,
    stable = occupancy[, "stable"],
    progressive = occupancy[, "progressive"],
    death = occupancy[, "death"],
    stable_unit_cost = stable_unit_cost,
    progressive_unit_cost = unit_costs$progressive,
    death_unit_cost = unit_costs$death
  )
  result$cost_stable <- result$stable * result$stable_unit_cost
  result$cost_progressive <- result$progressive * result$progressive_unit_cost
  result$cost_death <- result$death * result$death_unit_cost
  result$cost_cycle <- result$cost_stable + result$cost_progressive +
    result$cost_death
  result$cost_cumulative <- cumsum(result$cost_cycle)
  
  stopifnot(
    nrow(result) == 60L,
    all(abs(rowSums(occupancy) - 1) < 1e-12),
    all(occupancy >= -1e-12 & occupancy <= 1 + 1e-12),
    sum(cycles <= 4L) == 4L,
    sum(cycles >= 5L & cycles <= 7L) == 3L,
    sum(cycles >= 8L) == 53L,
    all(is.finite(result$cost_cycle))
  )
  result
}

cost_standard_tmz <- calculate_cycle_costs("Standard TMZ therapy")
cost_long_term_tmz <- calculate_cycle_costs("Long-term TMZ therapy")
cost_totals <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_cost = c(sum(cost_standard_tmz$cost_cycle),
                 sum(cost_long_term_tmz$cost_cycle))
)

# Display all cycles and totals; retain full precision for later calculations.
cat("\nStandard TMZ therapy: costs by cycle\n")
print(cost_standard_tmz, row.names = FALSE, digits = 10, max = 10000L)
cat("\nLong-term TMZ therapy: costs by cycle\n")
print(cost_long_term_tmz, row.names = FALSE, digits = 10, max = 10000L)
cat("\nTotal costs over cycles 1-60\n")
print(cost_totals, row.names = FALSE, digits = 10)

# Step 5: ICERs for Long-term TMZ therapy versus Standard TMZ therapy.
# Run tmz_markov_trace.R, tmz_qalms.R, and tmz_costs.R first.
# Reuses existing totals without redefining input data.
# IMPORTANT: Existing costs use initial-state cycle 1 (months 0-59), whereas
# existing QALMs use end-of-cycle occupancy (months 1-60). These ICERs use
# those totals as supplied; align timing before using them in an HTA submission.

if (!exists("cost_totals") || !exists("qalm_totals")) {
  stop("Run tmz_qalms.R and tmz_costs.R after tmz_markov_trace.R first.")
}

get_total <- function(table, treatment, column) {
  value <- table[table$treatment == treatment, column]
  if (length(value) != 1L || !is.finite(value)) {
    stop(paste("Missing, duplicated, or invalid total for", treatment, column))
  }
  value
}

incremental_cost <-
  get_total(cost_totals, "Long-term TMZ therapy", "total_cost") -
  get_total(cost_totals, "Standard TMZ therapy", "total_cost")
incremental_qalms <-
  get_total(qalm_totals, "Long-term TMZ therapy", "total_qalms") -
  get_total(qalm_totals, "Standard TMZ therapy", "total_qalms")
incremental_qalys <- incremental_qalms / 12

if (incremental_qalms == 0) {
  stop("ICERs are undefined because incremental health benefit is zero.")
}

# Keep full precision in calculations; round only the displayed ICERs.
icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys
icer_results <- data.frame(
  measure = c("Cost per QALM gained", "Cost per QALY gained"),
  incremental_cost = incremental_cost,
  incremental_effect = c(incremental_qalms, incremental_qalys),
  icer = c(icer_per_qalm, icer_per_qaly)
)
icer_display <- data.frame(
  measure = icer_results$measure,
  ICER = sprintf("%.0f", icer_results$icer)
)

cat("Long-term TMZ therapy versus Standard TMZ therapy\n")
cat("Provisional: cost and utility totals use different cycle timing.\n")
print(icer_display, row.names = FALSE, right = FALSE)