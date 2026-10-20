# Step 1: Store supplied inputs only. Sourcing this file produces no output.
# Cost currency and valuation year have not been supplied.
# Duration-dependent utility rules will be implemented in a later step.

tmz_inputs <- list(
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

# Step 1: Store supplied inputs only. Sourcing this file produces no output.
# Cost currency and valuation year have not been supplied.
# Duration-dependent utility rules will be implemented in a later step.

tmz_inputs <- list(
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

# Step 2: Calculate end-of-cycle cohort proportions for cycles 1 through 60.
# Assumptions: all patients are stable before cycle 1; probabilities are
# per month and constant; no recovery from progression; death is absorbing.
# The initial distribution is not included as a trace row.
# Utilities and costs do not affect state occupancy and are not applied here.

states <- c("stable", "progressive", "death")
initial_distribution <- c(stable = 1, progressive = 0, death = 0)
n_cycles <- as.integer(tmz_inputs$design$time_horizon_months /
                         tmz_inputs$design$cycle_length_months)
stopifnot(n_cycles == 60L)

make_transition_matrix <- function(probabilities) {
  p_sp <- unname(probabilities["stable_to_progressive"])
  p_sd <- unname(probabilities["stable_to_death"])
  p_pd <- unname(probabilities["progressive_to_death"])
  
  # Rows are origin states; columns are destination states.
  transition_matrix <- matrix(
    c(1 - p_sp - p_sd, p_sp, p_sd,
      0, 1 - p_pd, p_pd,
      0, 0, 1),
    nrow = 3L, byrow = TRUE,
    dimnames = list(from = states, to = states)
  )
  stopifnot(all(is.finite(transition_matrix)),
            all(transition_matrix >= 0),
            all(transition_matrix <= 1),
            all(abs(rowSums(transition_matrix) - 1) < 1e-12))
  transition_matrix
}

calculate_trace <- function(transition_matrix) {
  occupancy <- matrix(NA_real_, nrow = n_cycles, ncol = length(states),
                      dimnames = list(NULL, states))
  current_distribution <- initial_distribution
  
  # Exactly 60 transitions: row 1 follows the first monthly transition,
  # and row 60 follows the sixtieth monthly transition.
  for (cycle in seq_len(n_cycles)) {
    current_distribution <- as.numeric(current_distribution %*%
                                         transition_matrix)
    occupancy[cycle, ] <- current_distribution
  }
  
  trace <- data.frame(cycle = seq_len(n_cycles), occupancy,
                      row.names = NULL, check.names = FALSE)
  stopifnot(nrow(trace) == 60L,
            identical(trace$cycle, 1:60),
            all(is.finite(occupancy)),
            all(occupancy >= -1e-12),
            all(abs(rowSums(occupancy) - 1) < 1e-12),
            all(diff(trace$death) >= -1e-12))
  trace
}

transition_matrices <- lapply(tmz_inputs$transition_probabilities,
                              make_transition_matrix)
markov_traces <- lapply(transition_matrices, calculate_trace)

trace_standard_tmz <- markov_traces$standard_tmz
trace_long_term_tmz <- markov_traces$long_term_tmz

# Results remain in memory without automatic printing.
# To view results explicitly, run:
# print(trace_standard_tmz, row.names = FALSE)
# print(trace_long_term_tmz, row.names = FALSE)

# Step 3: Calculate QALMs using previously defined inputs and Markov traces.
# First run tmz_markov_trace.R in the same R session, then source this script.
# No input data are redefined here.
#
# Follow the requested MODEL-CYCLE schedule: progressive utility decreases
# in cycles 3:25 and remains at the cycle-25 value for cycles 26:60.
# This is not an individual time-since-progression calculation: all people
# in the progressive state receive the same utility in a given model cycle.
# The supplied decrement is subtracted from the utility weight. Multiplying
# utility-weighted occupancy by cycle length in months gives QALMs.
# Death utility is assumed to be zero. No discounting or half-cycle correction.

if (!exists("tmz_inputs") || !exists("trace_standard_tmz") ||
    !exists("trace_long_term_tmz")) {
  stop("Run tmz_markov_trace.R in this R session before running tmz_qalms.R.")
}

calculate_cycle_qalms <- function(trace) {
  stopifnot(nrow(trace) == 60L,
            identical(as.integer(trace$cycle), 1:60),
            all(c("stable", "progressive", "death") %in% names(trace)))
  
  # Zero decrements in cycles 1:2; 1:23 decrements in cycles 3:25;
  # 23 decrements thereafter. Utility is never reset after cycle 25.
  decrement_count <- pmin(pmax(trace$cycle - 2L, 0L), 23L)
  progressive_utility <- tmz_inputs$utilities$progressive -
    decrement_count *
    tmz_inputs$utilities$decrement_per_consecutive_month_of_progression
  
  results <- trace
  results$stable_utility <- tmz_inputs$utilities$stable
  results$progressive_utility <- progressive_utility
  results$death_utility <- 0
  results$stable_qalms <- results$stable * results$stable_utility *
    tmz_inputs$design$cycle_length_months
  results$progressive_qalms <- results$progressive *
    results$progressive_utility * tmz_inputs$design$cycle_length_months
  results$death_qalms <- results$death * results$death_utility *
    tmz_inputs$design$cycle_length_months
  results$cycle_qalms <- results$stable_qalms +
    results$progressive_qalms + results$death_qalms
  results$cumulative_qalms <- cumsum(results$cycle_qalms)
  
  stopifnot(sum(diff(decrement_count) == 1L) == 23L,
            all(progressive_utility[25:60] == progressive_utility[25]),
            all(is.finite(results$cycle_qalms)))
  results
}

qalms_standard_tmz <- calculate_cycle_qalms(trace_standard_tmz)
qalms_long_term_tmz <- calculate_cycle_qalms(trace_long_term_tmz)

total_qalms <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_qalms = c(sum(qalms_standard_tmz$cycle_qalms),
                  sum(qalms_long_term_tmz$cycle_qalms))
)

# Display exactly 60 cycle results for each treatment, followed by totals.
print(qalms_standard_tmz, row.names = FALSE, max = nrow(qalms_standard_tmz) *
        ncol(qalms_standard_tmz))
print(qalms_long_term_tmz, row.names = FALSE, max = nrow(qalms_long_term_tmz) *
        ncol(qalms_long_term_tmz))
print(total_qalms, row.names = FALSE)

total_qalys <- data.frame(
  treatment = total_qalms$treatment,
  total_qalys = total_qalms$total_qalms / 12
)

print(total_qalys, row.names = FALSE)

# Step 4: Calculate costs using existing inputs and end-of-cycle traces.
# Run tmz_markov_trace.R in the same R session before this script.
# No input data are redefined.
#
# Cost timing specified for this step:
# cycle 1 = initial distribution (month 0);
# cycles 2:60 = occupancy after months 1:59.
# Charge cycle 1 at the months 1:3 unit cost as explicitly requested.
# This timing differs from the earlier QALM/QALY calculations, which used
# occupancy after months 1:60. Existing traces and QALYs are not overwritten.
# Costs are undiscounted, with no half-cycle correction, per initial patient.

if (!exists("tmz_inputs") || !exists("initial_distribution") ||
    !exists("trace_standard_tmz") || !exists("trace_long_term_tmz")) {
  stop("Run tmz_markov_trace.R in this R session before running tmz_costs.R.")
}

calculate_cycle_costs <- function(end_cycle_trace, unit_costs) {
  state_names <- c("stable", "progressive", "death")
  stopifnot(nrow(end_cycle_trace) == 60L,
            identical(as.integer(end_cycle_trace$cycle), 1:60),
            all(state_names %in% names(initial_distribution)),
            all(state_names %in% names(end_cycle_trace)))
  
  # Retain exactly 60 rows: initial occupancy followed by months 1:59.
  occupancy <- rbind(
    initial_distribution[state_names],
    as.matrix(end_cycle_trace[1:59, state_names])
  )
  results <- data.frame(cycle = 1:60, month = 0:59, occupancy,
                        row.names = NULL, check.names = FALSE)
  
  results$stable_unit_cost <- ifelse(
    results$cycle <= 4L, unit_costs[["stable_months_1_to_3"]],
    ifelse(results$cycle <= 7L, unit_costs[["stable_months_4_to_6"]],
           unit_costs[["stable_subsequent_months"]])
  )
  results$progressive_unit_cost <- unit_costs[["progressive"]]
  results$death_unit_cost <- unit_costs[["death"]]
  
  results$stable_cost <- results$stable * results$stable_unit_cost
  results$progressive_cost <- results$progressive *
    results$progressive_unit_cost
  results$death_cost <- results$death * results$death_unit_cost
  results$cycle_cost <- results$stable_cost + results$progressive_cost +
    results$death_cost
  results$cumulative_cost <- cumsum(results$cycle_cost)
  
  stopifnot(nrow(results) == 60L,
            sum(results$cycle <= 4L) == 4L,
            sum(results$cycle >= 5L & results$cycle <= 7L) == 3L,
            sum(results$cycle >= 8L) == 53L,
            all(abs(rowSums(occupancy) - 1) < 1e-12),
            all(is.finite(results$cycle_cost)))
  results
}

costs_standard_tmz <- calculate_cycle_costs(
  trace_standard_tmz, tmz_inputs$costs$standard_tmz
)
costs_long_term_tmz <- calculate_cycle_costs(
  trace_long_term_tmz, tmz_inputs$costs$long_term_tmz
)

total_costs <- data.frame(
  treatment = c("Standard TMZ therapy", "Long-term TMZ therapy"),
  total_cost = c(sum(costs_standard_tmz$cycle_cost),
                 sum(costs_long_term_tmz$cycle_cost))
)

# Print every cycle for each treatment, then totals; retain full precision
# in the stored result objects for subsequent calculations.
cat("\nStandard TMZ therapy: costs by cycle\n")
print(costs_standard_tmz, row.names = FALSE,
      max = nrow(costs_standard_tmz) * ncol(costs_standard_tmz))
cat("\nLong-term TMZ therapy: costs by cycle\n")
print(costs_long_term_tmz, row.names = FALSE,
      max = nrow(costs_long_term_tmz) * ncol(costs_long_term_tmz))
cat("\nTotal costs\n")
print(total_costs, row.names = FALSE, digits = 10)

# Step 5: Incremental cost-effectiveness of long-term versus standard TMZ.
# Run tmz_markov_trace.R, tmz_qalms.R and tmz_costs.R first in the same session.
# Reuse existing totals without redefining input data.
# Note: existing costs use initial occupancy plus months 1:59, whereas
# existing QALMs use months 1:60. These ICERs preserve those requested totals;
# the timing conventions should be aligned before submission use.

if (!exists("total_costs") || !exists("total_qalms")) {
  stop("Run the trace, QALM and cost scripts before running tmz_icers.R.")
}

comparator <- "Standard TMZ therapy"
intervention <- "Long-term TMZ therapy"

get_treatment_total <- function(results, treatment_name, value_column) {
  matched <- which(results$treatment == treatment_name)
  if (length(matched) != 1L) {
    stop("Expected exactly one total for: ", treatment_name)
  }
  value <- results[[value_column]][matched]
  stopifnot(length(value) == 1L, is.finite(value))
  value
}

incremental_cost <-
  get_treatment_total(total_costs, intervention, "total_cost") -
  get_treatment_total(total_costs, comparator, "total_cost")
incremental_qalms <-
  get_treatment_total(total_qalms, intervention, "total_qalms") -
  get_treatment_total(total_qalms, comparator, "total_qalms")
incremental_qalys <- incremental_qalms / 12

if (incremental_qalms == 0) {
  stop("ICERs are undefined because incremental health benefit is zero.")
}

# Calculate with full precision; round only for display.
icer_per_qalm <- incremental_cost / incremental_qalms
icer_per_qaly <- incremental_cost / incremental_qalys

icer_results <- data.frame(
  outcome = c("Cost per QALM gained", "Cost per QALY gained"),
  incremental_cost = incremental_cost,
  incremental_effect = c(incremental_qalms, incremental_qalys),
  icer = c(icer_per_qalm, icer_per_qaly)
)

cat("Long-term TMZ therapy versus Standard TMZ therapy\n")
cat(sprintf("ICER per QALM gained: %.0f\n", icer_per_qalm))
cat(sprintf("ICER per QALY gained: %.0f\n", icer_per_qaly))