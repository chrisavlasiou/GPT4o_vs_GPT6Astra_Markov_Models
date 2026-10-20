# Step 1: Store model inputs without printing
inputs <- list(
  transition_probabilities = data.frame(
    treatment = c("Standard TMZ", "Long-term TMZ"),
    stable_to_progressive = c(0.0956, 0.0894),
    stable_to_death = c(0.0464, 0.0397),
    progressive_to_death = c(0.0861, 0.0689)
  ),
  
  utilities = list(
    stable = 0.743,
    progressive = 0.731,
    decrement_per_consecutive_month_of_progression = 0.02
  ),
  
  costs = data.frame(
    treatment = c("Standard TMZ", "Long-term TMZ"),
    stable_months_1_to_3 = c(1944, 1944),
    stable_months_4_to_6 = c(2453, 2453),
    stable_subsequent_months = c(350, 2453),
    progressive = c(2720, 2720),
    death = c(0, 0)
  ),
  
  design = list(
    states = c("stable", "progressive", "death"),
    cycle_length_months = 1L,
    time_horizon_months = 60L
  )
)

# Three-state TMZ Markov model: exactly 60 monthly transitions.
# Assumptions: all patients initially stable; probabilities are monthly;
# no recovery from progression; death is absorbing.
# Rows record END-of-cycle occupancy. Initial occupancy is not a trace row.
# Base R only. Inputs and results are stored without printing.

inputs <- list(
  transition_probabilities = data.frame(
    treatment = c("Standard TMZ", "Long-term TMZ"),
    stable_to_progressive = c(0.0956, 0.0894),
    stable_to_death = c(0.0464, 0.0397),
    progressive_to_death = c(0.0861, 0.0689)
  ),
  utilities = list(
    stable = 0.743,
    progressive = 0.731,
    decrement_per_consecutive_month_of_progression = 0.02
  ),
  costs = data.frame(
    treatment = c("Standard TMZ", "Long-term TMZ"),
    stable_months_1_to_3 = c(1944, 1944),
    stable_months_4_to_6 = c(2453, 2453),
    stable_subsequent_months = c(350, 2453),
    progressive = c(2720, 2720),
    death = c(0, 0)
  ),
  design = list(
    states = c("stable", "progressive", "death"),
    cycle_length_months = 1L,
    time_horizon_months = 60L
  )
)

states <- inputs$design$states
n_cycles <- as.integer(inputs$design$time_horizon_months /
                         inputs$design$cycle_length_months)
stopifnot(n_cycles == 60L)
initial_distribution <- c(stable = 1, progressive = 0, death = 0)

# Matrix rows are origin states; columns are destination states.
make_transition_matrix <- function(p) {
  P <- matrix(
    c(1 - p$stable_to_progressive - p$stable_to_death,
      p$stable_to_progressive, p$stable_to_death,
      0, 1 - p$progressive_to_death, p$progressive_to_death,
      0, 0, 1),
    nrow = 3L, byrow = TRUE,
    dimnames = list(from = states, to = states)
  )
  stopifnot(all(is.finite(P)), all(P >= 0), all(P <= 1),
            all(abs(rowSums(P) - 1) < 1e-12))
  P
}

calculate_trace <- function(P) {
  occupancy <- matrix(NA_real_, nrow = n_cycles, ncol = length(states),
                      dimnames = list(NULL, states))
  current <- initial_distribution[states]
  for (cycle in seq_len(n_cycles)) {
    current <- as.numeric(current %*% P)
    occupancy[cycle, ] <- current
  }
  trace <- data.frame(cycle = seq_len(n_cycles), occupancy,
                      row.names = NULL)
  stopifnot(nrow(trace) == 60L, identical(trace$cycle, 1:60),
            all(is.finite(occupancy)), all(occupancy >= 0),
            all(occupancy <= 1),
            all(abs(rowSums(occupancy) - 1) < 1e-10),
            all(diff(trace$death) >= -1e-12))
  trace
}

transition_matrices <- setNames(
  lapply(seq_len(nrow(inputs$transition_probabilities)), function(i) {
    make_transition_matrix(inputs$transition_probabilities[i, ])
  }),
  inputs$transition_probabilities$treatment
)
markov_traces <- lapply(transition_matrices, calculate_trace)
trace_standard <- markov_traces[["Standard TMZ"]]
trace_long_term <- markov_traces[["Long-term TMZ"]]

# Each trace has 60 rows and columns: cycle, stable, progressive, death.
# Utilities and costs are retained for subsequent modelling steps.
# Duration-dependent progression utility requires tracking time since
# progression in a later step; it does not affect these occupancy traces.

# Run after tmz_markov_trace.R. Uses existing inputs and traces;
# does not redefine the input data or recalculate state occupancy.
# This implements the requested calendar-cycle utility schedule for all
# patients currently progressive, rather than patient-specific time since
# progression. The stated 0.02 decrement is treated as a utility decrement.
# Death utility is assumed to be zero. No discounting or half-cycle correction.

stopifnot(exists("inputs"), exists("trace_standard"),
          exists("trace_long_term"))

calculate_qalms <- function(trace) {
  stopifnot(nrow(trace) == 60L,
            identical(as.integer(trace$cycle), seq_len(60L)),
            all(c("stable", "progressive", "death") %in% names(trace)))
  
  # Cycles 1-2: zero decrements; cycles 3-25: 1-23 decrements;
  # cycles 26-60: retain all 23 decrements.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- inputs$utilities$progressive -
    inputs$utilities$decrement_per_consecutive_month_of_progression *
    decrement_count
  
  results <- trace
  results$progressive_decrement_count <- decrement_count
  results$stable_utility <- inputs$utilities$stable
  results$progressive_utility <- progressive_utility
  results$death_utility <- 0
  
  # Proportion * utility * months per cycle = QALMs.
  results$qalm_stable <- results$stable * results$stable_utility *
    inputs$design$cycle_length_months
  results$qalm_progressive <- results$progressive *
    results$progressive_utility * inputs$design$cycle_length_months
  results$qalm_death <- results$death * results$death_utility *
    inputs$design$cycle_length_months
  results$qalm_total <- results$qalm_stable + results$qalm_progressive +
    results$qalm_death
  results$cumulative_qalms <- cumsum(results$qalm_total)
  
  stopifnot(all(is.finite(results$qalm_total)),
            all(decrement_count[3:25] == 1:23),
            all(progressive_utility[26:60] == progressive_utility[25]))
  results
}

qalms_standard <- calculate_qalms(trace_standard)
qalms_long_term <- calculate_qalms(trace_long_term)

total_qalms <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_qalms = c(sum(qalms_standard$qalm_total),
                  sum(qalms_long_term$qalm_total))
)

# Print all 60 cycles for each treatment, followed by the totals.
cat("\nStandard TMZ: QALMs by cycle\n")
print(qalms_standard, row.names = FALSE, max = 10000L)
cat("\nLong-term TMZ: QALMs by cycle\n")
print(qalms_long_term, row.names = FALSE, max = 10000L)
cat("\nTotal QALMs over 60 cycles\n")
print(total_qalms, row.names = FALSE)

total_qalys <- data.frame(
  treatment = total_qalms$treatment,
  total_qalys = total_qalms$total_qalms / 12
)

print(total_qalys, row.names = FALSE)

# Run after tmz_markov_trace.R. Reuses existing inputs and traces.
# Cost cycle 1 is initial occupancy; cycles 2-60 are months 1-59.
# Exactly 60 cost rows: initial occupancy + first 59 original trace rows.
# Existing end-of-cycle traces and QALY results are not modified.
# Costs are undiscounted; no half-cycle correction is applied.

stopifnot(exists("inputs"), exists("initial_distribution"),
          exists("trace_standard"), exists("trace_long_term"))

calculate_costs <- function(trace, treatment_name) {
  state_names <- inputs$design$states
  stopifnot(nrow(trace) == 60L,
            identical(as.integer(trace$cycle), seq_len(60L)))
  unit_costs <- inputs$costs[
    inputs$costs$treatment == treatment_name, , drop = FALSE
  ]
  stopifnot(nrow(unit_costs) == 1L)
  
  occupancy <- rbind(
    matrix(initial_distribution[state_names], nrow = 1L,
           dimnames = list(NULL, state_names)),
    as.matrix(trace[seq_len(59L), state_names, drop = FALSE])
  )
  results <- data.frame(cycle = seq_len(60L), month = 0:59,
                        occupancy, row.names = NULL)
  
  results$stable_unit_cost <- ifelse(
    results$cycle <= 4L, unit_costs$stable_months_1_to_3,
    ifelse(results$cycle <= 7L, unit_costs$stable_months_4_to_6,
           unit_costs$stable_subsequent_months)
  )
  results$progressive_unit_cost <- unit_costs$progressive
  results$death_unit_cost <- unit_costs$death
  
  # Expected cost per initial patient; monthly unit costs apply once per row,
  # including the initial-distribution row as explicitly requested.
  results$cost_stable <- results$stable * results$stable_unit_cost
  results$cost_progressive <- results$progressive *
    results$progressive_unit_cost
  results$cost_death <- results$death * results$death_unit_cost
  results$cost_total <- results$cost_stable + results$cost_progressive +
    results$cost_death
  results$cumulative_cost <- cumsum(results$cost_total)
  
  stopifnot(nrow(results) == 60L,
            all(abs(rowSums(occupancy) - 1) < 1e-10),
            all(is.finite(results$cost_total)),
            sum(results$cycle <= 4L) == 4L,
            sum(results$cycle >= 5L & results$cycle <= 7L) == 3L,
            sum(results$cycle >= 8L) == 53L)
  results
}

costs_standard <- calculate_costs(trace_standard, "Standard TMZ")
costs_long_term <- calculate_costs(trace_long_term, "Long-term TMZ")

total_costs <- data.frame(
  treatment = c("Standard TMZ", "Long-term TMZ"),
  total_cost = c(sum(costs_standard$cost_total),
                 sum(costs_long_term$cost_total))
)

# Print every cycle and totals. Calculations retain full numerical precision.
cat("\nStandard TMZ: costs by cycle\n")
print(costs_standard, row.names = FALSE, max = 10000L)
cat("\nLong-term TMZ: costs by cycle\n")
print(costs_long_term, row.names = FALSE, max = 10000L)
cat("\nTotal costs per initial patient\n")
print(total_costs, row.names = FALSE)

# Run after tmz_qalms.R and tmz_costs.R.
# Comparison: Long-term TMZ minus Standard TMZ.
# Uses existing totals without redefining inputs.
# IMPORTANT: existing costs cover initial occupancy + months 1-59,
# whereas existing QALMs cover end-of-month occupancy for months 1-60.
# These ICERs reproduce those totals; align timing before HTA interpretation.

stopifnot(exists("total_costs"), exists("total_qalms"))

get_total <- function(x, treatment_name, value_column) {
  value <- x[x$treatment == treatment_name, value_column]
  stopifnot(length(value) == 1L, is.numeric(value), is.finite(value))
  value
}

incremental_cost <-
  get_total(total_costs, "Long-term TMZ", "total_cost") -
  get_total(total_costs, "Standard TMZ", "total_cost")

incremental_qalms <-
  get_total(total_qalms, "Long-term TMZ", "total_qalms") -
  get_total(total_qalms, "Standard TMZ", "total_qalms")

incremental_qalys <- incremental_qalms / 12
if (incremental_qalms == 0) {
  stop("ICER is undefined because incremental health benefit is zero.")
}

# Retain full precision for calculations; round only for display.
icer_results <- data.frame(
  outcome = c("QALM", "QALY"),
  incremental_cost = incremental_cost,
  incremental_effect = c(incremental_qalms, incremental_qalys),
  icer = c(incremental_cost / incremental_qalms,
           incremental_cost / incremental_qalys)
)

icer_display <- data.frame(
  comparison = "Long-term TMZ versus Standard TMZ",
  measure = paste("Cost per additional", icer_results$outcome),
  ICER = formatC(icer_results$icer, format = "f", digits = 0, big.mark = ",")
)
print(icer_display, row.names = FALSE, quote = FALSE)