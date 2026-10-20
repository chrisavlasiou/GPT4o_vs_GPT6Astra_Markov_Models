# Step 1: Store supplied inputs only. Sourcing this script produces no output.
# Currency, discounting and other model assumptions are not yet specified.
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

# Step 1: Store supplied inputs only. Sourcing this script produces no output.
# Currency, discounting and other model assumptions are not yet specified.
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

# Step 2: End-of-cycle cohort traces over exactly 60 monthly transitions.
# Assumptions: all patients are stable before cycle 1; no return from
# progressive to stable; death is absorbing; probabilities are monthly
# and constant over time. Baseline is not a row in either output trace.
states <- c("stable", "progressive", "death")
initial_distribution <- c(stable = 1, progressive = 0, death = 0)
n_cycles <- as.integer(tmz_inputs$design$time_horizon_months /
                         tmz_inputs$design$cycle_length_months)
stopifnot(n_cycles == 60L)

make_transition_matrix <- function(stable_to_progressive,
                                   stable_to_death,
                                   progressive_to_death) {
  # Rows = origin states; columns = destination states.
  transition_matrix <- matrix(
    c(1 - stable_to_progressive - stable_to_death,
      stable_to_progressive, stable_to_death,
      0, 1 - progressive_to_death, progressive_to_death,
      0, 0, 1),
    nrow = 3L, byrow = TRUE,
    dimnames = list(from = states, to = states)
  )
  stopifnot(all(is.finite(transition_matrix)),
            all(transition_matrix >= 0), all(transition_matrix <= 1),
            all(abs(rowSums(transition_matrix) - 1) < 1e-12))
  transition_matrix
}

calculate_trace <- function(transition_matrix) {
  occupancy <- matrix(NA_real_, nrow = n_cycles, ncol = length(states),
                      dimnames = list(NULL, states))
  current_distribution <- initial_distribution
  for (cycle in seq_len(n_cycles)) {
    current_distribution <- as.numeric(current_distribution %*%
                                         transition_matrix)
    occupancy[cycle, ] <- current_distribution
  }
  trace <- data.frame(cycle = seq_len(n_cycles), occupancy,
                      row.names = NULL)
  stopifnot(nrow(trace) == 60L,
            identical(trace$cycle, 1:60),
            all(occupancy >= -1e-12), all(occupancy <= 1 + 1e-12),
            all(abs(rowSums(occupancy) - 1) < 1e-12),
            all(diff(trace$death) >= -1e-12))
  trace
}

probabilities <- tmz_inputs$transition_probabilities
transition_matrices <- setNames(vector("list", nrow(probabilities)),
                                probabilities$treatment)
for (i in seq_len(nrow(probabilities))) {
  transition_matrices[[i]] <- make_transition_matrix(
    probabilities$stable_to_progressive[i],
    probabilities$stable_to_death[i],
    probabilities$progressive_to_death[i]
  )
}
markov_traces <- lapply(transition_matrices, calculate_trace)
trace_standard_tmz <- markov_traces[["Standard TMZ therapy"]]
trace_long_term_tmz <- markov_traces[["Long-term TMZ therapy"]]

# Results remain in the objects above without automatic printing.
# To inspect a trace interactively, run: View(trace_standard_tmz)

# Step 3: Run after tmz_markov_trace.R in the same R session.
# Reuses tmz_inputs and markov_traces; does not redefine input data.
# Uses end-of-cycle occupancy for cycles 1-60 with no half-cycle correction.
# Death utility is assumed to be zero.
# The supplied 0.02 is interpreted as a utility decrement, consistent with
# the original utility input. Utility x months yields QALMs (not QALYs).
# This is the requested model-cycle schedule, not a patient-duration schedule:
# all progressive occupants in a given cycle receive the same utility.

if (!exists("tmz_inputs") || !exists("markov_traces")) {
  stop("Run tmz_markov_trace.R first in the same R session.")
}

calculate_qalms <- function(trace) {
  stopifnot(nrow(trace) == 60L,
            identical(as.integer(trace$cycle), seq_len(60L)))
  
  # Zero decrements in cycles 1-2; 1-23 decrements in cycles 3-25;
  # exactly 23 decrements remain in effect throughout cycles 26-60.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- tmz_inputs$utilities$progressive -
    tmz_inputs$utilities$decrement_per_consecutive_month_of_progression *
    decrement_count
  
  result <- trace
  result$stable_utility <- tmz_inputs$utilities$stable
  result$progressive_utility <- progressive_utility
  result$death_utility <- 0
  result$stable_qalms <- result$stable * result$stable_utility *
    tmz_inputs$design$cycle_length_months
  result$progressive_qalms <- result$progressive *
    result$progressive_utility * tmz_inputs$design$cycle_length_months
  result$death_qalms <- result$death * result$death_utility *
    tmz_inputs$design$cycle_length_months
  result$cycle_qalms <- result$stable_qalms + result$progressive_qalms +
    result$death_qalms
  result$cumulative_qalms <- cumsum(result$cycle_qalms)
  
  stopifnot(sum(diff(decrement_count) == 1L) == 23L,
            all(decrement_count[1:2] == 0L),
            all(decrement_count[25:60] == 23L),
            all(result$progressive_utility[26:60] ==
                  result$progressive_utility[25]))
  result
}

qalm_results <- lapply(markov_traces, calculate_qalms)
qalms_standard_tmz <- qalm_results[["Standard TMZ therapy"]]
qalms_long_term_tmz <- qalm_results[["Long-term TMZ therapy"]]

total_qalms <- data.frame(
  treatment = names(qalm_results),
  total_qalms = vapply(qalm_results, function(x) sum(x$cycle_qalms), numeric(1)),
  row.names = NULL
)

# Print all 60 cycle results per treatment, followed by treatment totals.
# Input data are not printed separately. Objects retain full precision.
for (treatment in names(qalm_results)) {
  cat("\n", treatment, "\n", sep = "")
  print(qalm_results[[treatment]], row.names = FALSE, digits = 8,
        max = nrow(qalm_results[[treatment]]) * ncol(qalm_results[[treatment]]))
}
cat("\nTotal QALMs over cycles 1-60\n")
print(total_qalms, row.names = FALSE, digits = 10)

total_qalms$total_qalys <- total_qalms$total_qalms / 12
print(total_qalms)

# Step 4: Run after tmz_markov_trace.R in the same R session.
# Reuses existing inputs, initial_distribution and transition_matrices.
# Cost timing requested here differs from the earlier end-of-cycle traces:
# cycle 1 = initial distribution; cycles 2-60 = months 1-59.
# Exactly 60 cost rows are included, comprising baseline and 59 transitions.
# Costs are undiscounted, per initial cohort member, with no half-cycle correction.
# Earlier trace and QALM objects are not changed by this script.

if (!exists("tmz_inputs") || !exists("initial_distribution") ||
    !exists("transition_matrices")) {
  stop("Run tmz_markov_trace.R first in the same R session.")
}

calculate_cycle_costs <- function(treatment) {
  cycle <- seq_len(60L)
  state_names <- c("stable", "progressive", "death")
  occupancy <- matrix(NA_real_, nrow = length(cycle), ncol = 3L,
                      dimnames = list(NULL, state_names))
  occupancy[1L, ] <- initial_distribution[state_names]
  transition_matrix <- transition_matrices[[treatment]][state_names, state_names]
  for (i in 2:60) {
    occupancy[i, ] <- occupancy[i - 1L, ] %*% transition_matrix
  }
  
  cost_input <- tmz_inputs$costs[
    tmz_inputs$costs$treatment == treatment, , drop = FALSE
  ]
  stopifnot(nrow(cost_input) == 1L)
  
  stable_unit_cost <- ifelse(
    cycle <= 4L, cost_input$stable_months_1_to_3,
    ifelse(cycle <= 7L, cost_input$stable_months_4_to_6,
           cost_input$stable_subsequent_months)
  )
  
  result <- data.frame(
    cycle = cycle,
    month = cycle - 1L,
    occupancy,
    stable_unit_cost = stable_unit_cost,
    progressive_unit_cost = cost_input$progressive,
    death_unit_cost = cost_input$death,
    row.names = NULL
  )
  # Unit costs apply once per monthly cycle, including the requested baseline.
  result$stable_cost <- result$stable * result$stable_unit_cost
  result$progressive_cost <- result$progressive * result$progressive_unit_cost
  result$death_cost <- result$death * result$death_unit_cost
  result$cycle_cost <- result$stable_cost + result$progressive_cost +
    result$death_cost
  result$cumulative_cost <- cumsum(result$cycle_cost)
  
  stopifnot(nrow(result) == 60L,
            identical(result$cycle, seq_len(60L)),
            all(abs(rowSums(occupancy) - 1) < 1e-12),
            all(occupancy >= -1e-12),
            all(occupancy[1L, ] == initial_distribution[state_names]),
            sum(cycle <= 4L) == 4L,
            sum(cycle >= 5L & cycle <= 7L) == 3L,
            sum(cycle >= 8L) == 53L)
  result
}

cost_results <- setNames(
  lapply(names(transition_matrices), calculate_cycle_costs),
  names(transition_matrices)
)
costs_standard_tmz <- cost_results[["Standard TMZ therapy"]]
costs_long_term_tmz <- cost_results[["Long-term TMZ therapy"]]

total_costs <- data.frame(
  treatment = names(cost_results),
  total_cost = vapply(cost_results, function(x) sum(x$cycle_cost), numeric(1)),
  row.names = NULL
)

# Show every cycle and the totals; retain full precision in stored objects.
for (treatment in names(cost_results)) {
  cat("\n", treatment, "\n", sep = "")
  print(cost_results[[treatment]], row.names = FALSE, digits = 10,
        max = nrow(cost_results[[treatment]]) * ncol(cost_results[[treatment]]))
}
cat("\nTotal costs over cycles 1-60\n")
print(total_costs, row.names = FALSE, digits = 12)

# Step 5: Run after tmz_qalms.R and tmz_costs.R in the same R session.
# Reuses existing totals without redefining input data.
# Comparison: Long-term TMZ therapy minus Standard TMZ therapy.
# IMPORTANT: Current cost totals use baseline plus 59 transitions, whereas
# QALM totals use 60 end-of-cycle distributions. These ICERs reproduce those
# totals; align timing before using the results in an HTA submission.

if (!exists("total_costs") || !exists("total_qalms")) {
  stop("Run tmz_qalms.R and tmz_costs.R first in the same R session.")
}

comparator <- "Standard TMZ therapy"
intervention <- "Long-term TMZ therapy"

get_total <- function(results, treatment_name, value_column) {
  selected <- results[results$treatment == treatment_name, , drop = FALSE]
  if (nrow(selected) != 1L || !is.finite(selected[[value_column]])) {
    stop("Each treatment must have exactly one finite total.")
  }
  selected[[value_column]]
}

incremental_cost <-
  get_total(total_costs, intervention, "total_cost") -
  get_total(total_costs, comparator, "total_cost")
incremental_qalms <-
  get_total(total_qalms, intervention, "total_qalms") -
  get_total(total_qalms, comparator, "total_qalms")
incremental_qalys <- incremental_qalms / 12

if (incremental_qalms == 0) {
  stop("ICERs are undefined because incremental health benefit is zero.")
}

# Keep full precision for calculations; round only the displayed ICERs.
icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys

icer_results <- data.frame(
  outcome = c("QALM", "QALY"),
  incremental_cost = incremental_cost,
  incremental_effect = c(incremental_qalms, incremental_qalys),
  icer = c(icer_per_qalm, icer_per_qaly)
)

cat("Long-term TMZ therapy versus Standard TMZ therapy\n")
cat(sprintf("ICER per QALM: %.0f\n", icer_per_qalm))
cat(sprintf("ICER per QALY: %.0f\n", icer_per_qaly))