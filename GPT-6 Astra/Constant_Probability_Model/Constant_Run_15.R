# Store model inputs without printing
inputs <- list(
  transition_probabilities = list(
    standard_tmz = c(
      stable_to_progressive = 0.0956,
      stable_to_death = 0.0464,
      progressive_to_death = 0.0861
    ),
    long_term_tmz = c(
      stable_to_progressive = 0.0894,
      stable_to_death = 0.0397,
      progressive_to_death = 0.0689
    )
  ),
  
  utilities = list(
    stable = 0.743,
    progressive = 0.731,
    decrement_per_consecutive_month_of_progression = 0.02
  ),
  
  costs = list(
    standard_tmz = c(
      stable_months_1_to_3 = 1944,
      stable_months_4_to_6 = 2453,
      stable_subsequent_months = 350,
      progressive = 2720,
      death = 0
    ),
    long_term_tmz = c(
      stable_months_1_to_3 = 1944,
      stable_months_4_to_6 = 2453,
      stable_subsequent_months = 2453,
      progressive = 2720,
      death = 0
    )
  ),
  
  design = list(
    cycle_length_months = 1,
    time_horizon_months = 60
  )
)

# Three-state TMZ cohort Markov model: exactly 60 monthly cycles.
# Assumptions: everyone starts stable; probabilities apply per month;
# no return from progressive to stable; death is absorbing.
# Row 1 is the END of month 1, after one transition.
# The initial distribution is kept separately and is not an extra trace row.

transition_probabilities <- list(
  standard_tmz = c(stable_to_progressive = 0.0956,
                   stable_to_death = 0.0464,
                   progressive_to_death = 0.0861),
  long_term_tmz = c(stable_to_progressive = 0.0894,
                    stable_to_death = 0.0397,
                    progressive_to_death = 0.0689)
)
cycle_length_months <- 1L
time_horizon_months <- 60L
n_cycles <- as.integer(time_horizon_months / cycle_length_months)
states <- c("stable", "progressive", "death")
initial_distribution <- c(stable = 1, progressive = 0, death = 0)

make_transition_matrix <- function(p) {
  # Rows: current state; columns: next state.
  P <- matrix(c(
    1 - p["stable_to_progressive"] - p["stable_to_death"],
    p["stable_to_progressive"], p["stable_to_death"],
    0, 1 - p["progressive_to_death"], p["progressive_to_death"],
    0, 0, 1
  ), nrow = 3L, byrow = TRUE, dimnames = list(states, states))
  stopifnot(all(is.finite(P)), all(P >= 0), all(P <= 1),
            all(abs(rowSums(P) - 1) < 1e-12))
  P
}

calculate_trace <- function(P) {
  occupancy <- matrix(NA_real_, nrow = n_cycles, ncol = length(states),
                      dimnames = list(NULL, states))
  current <- initial_distribution
  for (cycle in seq_len(n_cycles)) {
    current <- as.numeric(current %*% P)
    occupancy[cycle, ] <- current
  }
  trace <- data.frame(cycle = seq_len(n_cycles), occupancy,
                      row.names = NULL)
  stopifnot(nrow(trace) == 60L,
            identical(trace$cycle, seq_len(60L)),
            all(abs(rowSums(occupancy) - 1) < 1e-12),
            all(occupancy >= -1e-12), all(occupancy <= 1 + 1e-12),
            all(diff(trace$death) >= -1e-12))
  trace
}

transition_matrices <- lapply(transition_probabilities, make_transition_matrix)
markov_traces <- lapply(transition_matrices, calculate_trace)
trace_standard_tmz <- markov_traces$standard_tmz
trace_long_term_tmz <- markov_traces$long_term_tmz

# Inputs and results are stored silently. To inspect results, run:
# print(trace_standard_tmz, row.names = FALSE)
# print(trace_long_term_tmz, row.names = FALSE)

# Run after the existing inputs definition and tmz_markov_trace.R.
# Reuses inputs, trace_standard_tmz, and trace_long_term_tmz;
# no input data are redefined.
# Uses end-of-cycle occupancy, with no discounting or half-cycle correction.
# The 0.02 input is interpreted as a utility decrement, not a literal
# annual QALY loss. Utility * occupancy * months gives QALMs.
# This implements the requested MODEL-CYCLE schedule, rather than tracking
# each patient's time since progression: all progressive patients in a given
# model cycle receive the same utility, including newly progressed patients.
# Death contributes zero QALMs.

stopifnot(exists("inputs"), exists("trace_standard_tmz"),
          exists("trace_long_term_tmz"))

calculate_qalms <- function(trace) {
  stopifnot(nrow(trace) == 60L,
            identical(as.integer(trace$cycle), seq_len(60L)))
  
  # Zero decrements in cycles 1-2; 1-23 decrements in cycles 3-25;
  # exactly 23 decrements for all subsequent cycles.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- inputs$utilities$progressive -
    inputs$utilities$decrement_per_consecutive_month_of_progression *
    decrement_count
  
  results <- trace
  results$progressive_utility <- progressive_utility
  results$qalm_stable <- trace$stable * inputs$utilities$stable *
    inputs$design$cycle_length_months
  results$qalm_progressive <- trace$progressive * progressive_utility *
    inputs$design$cycle_length_months
  results$qalm_death <- numeric(nrow(trace))
  results$qalm_total <- results$qalm_stable + results$qalm_progressive +
    results$qalm_death
  results$cumulative_qalms <- cumsum(results$qalm_total)
  
  stopifnot(all(decrement_count[1:2] == 0L),
            identical(as.integer(decrement_count[3:25]), seq_len(23L)),
            all(progressive_utility[26:60] == progressive_utility[25]))
  results
}

qalms_standard_tmz <- calculate_qalms(trace_standard_tmz)
qalms_long_term_tmz <- calculate_qalms(trace_long_term_tmz)

total_qalms <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_qalms = c(sum(qalms_standard_tmz$qalm_total),
                  sum(qalms_long_term_tmz$qalm_total))
)

# Show every cycle for both treatments, followed by totals.
print(qalms_standard_tmz, row.names = FALSE, max = 10000)
print(qalms_long_term_tmz, row.names = FALSE, max = 10000)
print(total_qalms, row.names = FALSE)

total_qalys <- data.frame(
  treatment = total_qalms$treatment,
  total_qalys = total_qalms$total_qalms / 12
)

print(total_qalys, row.names = FALSE)


# Run after the original inputs definition and tmz_markov_trace.R.
# Reuses existing data without redefining costs or transition probabilities.
# Cost timing requested here differs from the earlier QALM timing:
# cycle 1 = initial distribution; cycles 2-60 = months 1-59.
# Exactly 60 cost rows: initial distribution plus 59 transitions.
# Costs are undiscounted. No half-cycle correction is applied.
# Original traces and QALM/QALY results are not modified.

stopifnot(exists("inputs"), exists("initial_distribution"),
          exists("trace_standard_tmz"), exists("trace_long_term_tmz"))

make_cost_trace <- function(end_of_cycle_trace) {
  states <- names(initial_distribution)
  stopifnot(nrow(end_of_cycle_trace) == 60L,
            identical(as.integer(end_of_cycle_trace$cycle), seq_len(60L)))
  occupancy <- rbind(
    initial_distribution,
    as.matrix(end_of_cycle_trace[seq_len(59L), states, drop = FALSE])
  )
  trace <- data.frame(cycle = seq_len(60L), occupancy, row.names = NULL)
  stopifnot(nrow(trace) == 60L,
            all(abs(rowSums(occupancy) - 1) < 1e-12),
            all(occupancy >= 0))
  trace
}

calculate_costs <- function(trace, unit_costs) {
  results <- trace
  results$month <- results$cycle - 1L
  results$unit_cost_stable <- ifelse(
    results$cycle <= 4L,
    unit_costs["stable_months_1_to_3"],
    ifelse(results$cycle <= 7L,
           unit_costs["stable_months_4_to_6"],
           unit_costs["stable_subsequent_months"])
  )
  results$unit_cost_progressive <- unname(unit_costs["progressive"])
  results$unit_cost_death <- unname(unit_costs["death"])
  results$cost_stable <- results$stable * results$unit_cost_stable
  results$cost_progressive <- results$progressive * results$unit_cost_progressive
  results$cost_death <- results$death * results$unit_cost_death
  results$cost_total <- results$cost_stable + results$cost_progressive +
    results$cost_death
  results$cumulative_cost <- cumsum(results$cost_total)
  stopifnot(sum(results$cycle <= 4L) == 4L,
            sum(results$cycle >= 5L & results$cycle <= 7L) == 3L,
            sum(results$cycle >= 8L) == 53L)
  results
}

cost_trace_standard_tmz <- make_cost_trace(trace_standard_tmz)
cost_trace_long_term_tmz <- make_cost_trace(trace_long_term_tmz)

costs_standard_tmz <- calculate_costs(
  cost_trace_standard_tmz, inputs$costs$standard_tmz
)
costs_long_term_tmz <- calculate_costs(
  cost_trace_long_term_tmz, inputs$costs$long_term_tmz
)

total_costs <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_cost = c(sum(costs_standard_tmz$cost_total),
                 sum(costs_long_term_tmz$cost_total))
)

# Display all 60 cycles and total costs per initial cohort member.
# Values retain full precision for subsequent calculations.
print(costs_standard_tmz, row.names = FALSE, max = 10000)
print(costs_long_term_tmz, row.names = FALSE, max = 10000)
print(total_costs, row.names = FALSE)


# Run after tmz_qalms.R and tmz_costs.R.
# Reuses existing totals; input data are not redefined.
# Comparison: Long-term TMZ therapy minus Standard TMZ therapy.
# Timing note: existing costs use the initial distribution + 59 transitions,
# whereas existing QALMs use 60 end-of-cycle distributions. This script
# reports the ICERs from those totals; it does not harmonise their timing.

stopifnot(exists("total_costs"), exists("total_qalms"))
treatments <- c("Standard TMZ therapy", "Long-term TMZ therapy")
stopifnot(!anyDuplicated(total_costs$treatment),
          !anyDuplicated(total_qalms$treatment))
cost_index <- match(treatments, total_costs$treatment)
qalm_index <- match(treatments, total_qalms$treatment)
stopifnot(!anyNA(cost_index), !anyNA(qalm_index))

costs <- total_costs$total_cost[cost_index]
qalms <- total_qalms$total_qalms[qalm_index]
stopifnot(all(is.finite(costs)), all(is.finite(qalms)))
qalys <- qalms / 12

incremental_cost <- costs[2] - costs[1]
incremental_qalms <- qalms[2] - qalms[1]
incremental_qalys <- qalys[2] - qalys[1]
if (incremental_qalms == 0 || incremental_qalys == 0) {
  stop("ICER is undefined because incremental effectiveness is zero.")
}

# Retain unrounded numeric results for subsequent analysis.
icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys

# Show the ICERs without decimal places.
cat("Long-term TMZ versus Standard TMZ\n")
cat(sprintf("ICER per QALM: %.0f\n", icer_per_qalm))
cat(sprintf("ICER per QALY: %.0f\n", icer_per_qaly))